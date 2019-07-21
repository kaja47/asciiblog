package asciiblog

import java.lang.StringBuilder
import scala.collection.mutable
import MakeFiles.{ licenses, peelOffTags, isAbsolute }
import MarkupParser.{ findMods, mkMods }
import AsciiPatterns._


trait Markup {
  def process(text: Seq[String], imageRoot: String): Text
  def empty: Text
}

trait Text {
  def render(relativize: String => String): String
  def plaintextSummary: String
  def plaintext: String
  def images: Seq[Image]
  def links: Seq[String]

  def resolve(resolver: LinkResolver): Text
  def appendImages(images: Seq[Image]): Text
  def mapImages(f: Image => Image): Text
}



// Object passed to Text in order to properly render local links as absolute urls
trait LinkResolver {
  def link(l: String): String
  def thumbnail(img: Image): String
  def bigThumbnail(img: Image, half: Boolean): String
  def tag(t: Tag): String
}

object LinkResolver {
  val identity = new LinkResolver {
    def link(l: String): String = l
    def thumbnail(img: Image): String = img.url
    def bigThumbnail(img: Image, half: Boolean): String = img.url
    def tag(t: Tag): String = ???
  }
}



// ASCII markup

object AsciiPatterns {
  val linkRegex    = """(?x)  " ([^"]+?) " : \[ ([^\]\n]+?) \]""".r
  val ahrefRegex   = """(?x) (?<= href=") (.*?) (?=") """.r

  val linkCheck    = "\":["
  val ahrefCheck   = "href="

  val noteRegex    = """(?x) \[\[ (\d++) \]\]""".r

  val linkRefRegex = """(?xm) ^\[(.*?)\]:\ +(.+)$""".r
  val hrRegex      = """(?xm) ---+|\*\*\*+ """.r
  val headingRegex = """(?xm) ---+|\*\*\*+|===+|\#\#\#+ """.r
  val definitionListRegex = """^\s++-\s++(.*+)$""".r
  val commentRegex = """(?xs) \<!--.*?--\>""".r

  val linkUntil = Slurp { _.until('"') }
  val linkSlurp = Slurp { _.groupStart(0).quoted('"').asGroup(1, 1, -1).char(':').ignore().delimited('[', ']').asGroup(2, 1, -1).groupEnd(0) }

  def linkSlurpIterator(txt: String) =
    Slurp(txt, groups = 3).iterator(linkUntil, linkSlurp).map(_.group(2).asString())
  def linkSlurpReplace(txt: String)(f: Slurp.Replacement) =
    Slurp(txt, groups = 3).replace(linkUntil, linkSlurp, f)

  private val stripTagRegex = """\<.*?\>""".r // TODO less crude way to strip tags

  def stripTags(html: String) = {
    var t = html
    t = (if (html.indexOf('<') == -1) html else stripTagRegex.replaceAllIn(html, ""))
    t = t.replace("\u00AD", "")  // soft hyphen
    t = t.replace("\u00A0", " ") // nbsp
    t = t.replace("&shy;",  "")  // soft hyphen
    t = t.replace("&nbsp;", " ") // nbsp
    //t = t.replaceAll("\\s+", " ")
    //t

    val res = new StringBuilder(t.length)
    var space = true
    var i = 0; while (i < t.length) {
      val ch = t.charAt(i)
      val ws = Character.isWhitespace(ch)

      if (!ws) res.append(ch)
      else if (!space) res.append(' ')

      space = ws
      i += 1
    }

    res.toString
  }
}




case class AsciiText(segments: Seq[Segment], parser: MarkupParser, highlighter: Highlighters, resolver: LinkResolver = null) extends Text { self =>

  def render(relativize: String => String): String = mkText(segments, resolvedLinks, relativize)
  def plaintextSummary: String = stripTags(mkParagraph(segments.collect { case Paragraph(txt, _) => txt }.headOption.getOrElse(""), resolvedLinks, identity, true))
  def plaintext: String = stripTags(processTexts(segments, txt => Iterator(mkParagraph(txt, resolvedLinks, identity, true))).mkString(" "))

  def resolve(r: LinkResolver): AsciiText = copy(resolver = r)
  def appendImages(images: Seq[Image]): Text = copy(segments = segments :+ Images(images))
  def mapImages(f: Image => Image): Text = {
    def mapSegments(ss: Seq[Segment]): Seq[Segment] = ss.map {
      case Images(is)     => Images(is.map(f))
      case Blockquote(ss) => Blockquote(mapSegments(ss))
      case s => s
    }
    // Overwrites segments, doesn't resolve links again. This saves some work but
    // mainly it's there so error messages during link resolution are not
    // displayed twice
    new AsciiText(mapSegments(segments), parser, highlighter, resolver) {
      override protected def resolvedLinks = self.resolvedLinks
    }
  }

  val images: Seq[Image] = segments.collect { case Images(imgs) => imgs }.flatten

  private def processTexts[T](segments: Seq[Segment], f: String => Iterator[T]): Iterator[T] = segments.iterator.flatMap {
    case t: Textual     => f(t.txt)
    case Images(images) => images.iterator.flatMap(i => f(i.title))
    case Blockquote(sx) => processTexts(sx, f)
    case SegmentSeq(sx) => processTexts(sx, f)
    case BulletList(items)   => processTexts(items, f)
    case NumberedList(items) => processTexts(items.map(_._2), f)
    case DefinitionList(items) => processTexts(items.flatMap { case (dt, dds) => dt +: dds }, f)
    case Table(rows)         => rows.iterator.flatten.flatMap(cell => f(cell.txt))
    case _ => Iterator()
  }

  private def extractLinks(txt: String): Iterator[String] = // 100 ms
    (if (txt.contains(ahrefCheck)) ahrefRegex.findAllMatchIn(txt).map(_.group(1)) else Iterator()) ++
    (if (txt.contains(linkCheck))  linkSlurpIterator(txt)                         else Iterator())

  private def checkAliases(aliases: Iterator[(String, String)]) = {
    val as = mutable.Map[String, String]()
    for ((l, url) <- aliases) {
      if (as.contains(l))      sys.error(s"duplicate link refs [$l]")
      if (url.startsWith("?")) sys.error(s"undefined link ref $l -> $url (link prefixed by ??? is placeholder for missing url)")
      as.update(l, url)
    }
    as
  }

  protected def resolvedLinks = _resolvedLinks
  private lazy val _resolvedLinks = {
    val aliases = segments.iterator.collect { case Linkref(lm) => lm }.flatten
    val aliasMap = checkAliases(aliases)
    processTexts(segments, extractLinks).map { l =>
      val (base, hash) = util.splitByHash(l)
      (l, resolver.link(aliasMap.getOrElse(base, base)+hash))
    }.toMap
  }

  lazy val links: Seq[String] = resolvedLinks.valuesIterator.toVector

  private lazy val validRefTargets: Map[Int, NumberedList] = {
    val refLinks = processTexts(segments, txt => noteRegex.findAllMatchIn(txt).map(_.group(1).toInt)).toSet

    val refTargets = segments.collect { case NumberedList(items) => items.map(_._1) }.flatten.toSet
    refLinks.foreach { num =>
      if (!refTargets.contains(num)) sys.error(s"invalid reference $num")
    }

    segments.flatMap {
      case list @ NumberedList(items) =>
        items.map { case (num, _) => (num, list) }
      case _ => Seq()
    }.toMap // last key should be used
  }

  private def mkParagraph(txt: String, aliases: Map[String, String], relativize: String => String, plaintext: Boolean = false): String =
    parser(txt, (link: String) => {
      val al = aliases(link)
      if (al == Blog.invalidLinkMarker) null else relativize(al)
    }, plaintext)

  def mkText(segments: Seq[Segment], aliases: Map[String, String], relativize: String => String): String =
    _mkText(segments, aliases, relativize, new StringBuilder(1024))

  private def _mkText(segments: Seq[Segment], aliases: Map[String, String], relativize: String => String, sb: StringBuilder): String = {
    for (i <- 0 until segments.length) {
      def omitEndPTag = if (i == segments.length-1) false else (segments(i+1) match {
        case Paragraph(_, _) | Table(_) | Hr() | Blockquote(_) | Heading(_, _, _) | Block("pre", _, _) | Block("div", _, _) | BulletList(_) | NumberedList(_) => true
        case _ => false
      })

      segments(i) match {
      case Heading(level, txt, mods)   => sb.append("<h").append(level).append(mkMods(mods)).append(">").append(mkParagraph(txt, aliases, relativize)).append("</h").append(level).append(">")
      case Hr()                 => sb.append("<hr>\n")
      case Linkref(_)           =>
      case Block("html", txt, _)    => sb.append(txt)
      case Block("text", txt, _)    => sb.append(html.escape(txt))
      case Block("div",  txt, mods) => sb.append("<div").append(mkMods(mods)).append(">").append(txt).append("</div>")
      case Block("pre",  txt, mods) => sb.append("<pre").append(mkMods(mods)).append(">").append(html.escape(txt)).append("</pre>")
      case Block("comment", _, _)   =>
      case Block(tpe, _, _)        => sys.error(s"unknown block type '$tpe'")
      case Code(lang, txt, mods) =>
        sb.append("<pre").append(mkMods(mods)).append(">")
        if (lang.nonEmpty) {
          sb.append(highlighter.highlight(txt, lang))
        } else {
          sb.append(html.escape(txt))
        }
        sb.append("</pre>")
      case Images(images)       => images.foreach { img => sb.append(mkImgTag(img, aliases, relativize, self.images.head == img)).append(" ") }
      case Paragraph(txt, mods) =>
        sb.append("<p").append(mkMods(mods)).append(">").append(mkParagraph(txt, aliases, relativize))
        if (!omitEndPTag) sb.append("</p>")
      case Blockquote(sx)       => sb.append("<blockquote>"); _mkText(sx, aliases, relativize, sb); sb.append("</blockquote>")
      case Inline(txt)          => sb.append(mkParagraph(txt, aliases, relativize))
      case ByLine(txt)          => sb.append("<div style=text-align:right>").append(mkParagraph(txt, aliases, relativize)).append("</div>")
      case SegmentSeq(sx)       => _mkText(sx, aliases, relativize, sb)
      case BulletList(items)    =>
        sb.append("<ul>")
        items.foreach { it =>
          sb.append("<li>")
          _mkText(Seq(it), aliases, relativize, sb)
          sb.append("\n")
        }
        sb.append("</ul>")

      case list @ NumberedList(items) =>
        sb.append("<ol>")
        items.zipWithIndex.foreach {
          case ((num, it), i) =>
            sb.append("<li")
            if (validRefTargets(num) == list) { sb.append(" id=").append(html.quoteAttribute("fn"+num)) }
            if (num != i+1)                   { sb.append(" value=").append(html.quoteAttribute(num.toString)) }
            sb.append(">")
            _mkText(Seq(it), aliases, relativize, sb)
            sb.append("\n")
        }
        sb.append("</ol>")

      case DefinitionList(items) =>
        sb.append("<dl>\n")
        items.foreach { case (dt, dls) =>
          sb.append("<dt>")
          _mkText(Seq(dt), aliases, relativize, sb)
          dls.foreach { dl =>
            sb.append("<dd>")
            _mkText(Seq(dl), aliases, relativize, sb)
            sb.append("\n")
          }
        }
        sb.append("</dl>")

      case Table(rows) =>
        sb.append("<table>")
        rows.foreach { cols =>
          sb.append("<tr>")
          cols.foreach { case Cell(txt, cols, rows, th) =>
            sb.append(if (th) "<th" else "<td")
            if (cols > 1) sb.append(" colspan=").append(cols)
            if (rows > 1) sb.append(" rowspan=").append(rows)
            sb.append(">").append(mkParagraph(txt.trim, aliases, relativize))
          }
          sb.append("\n")
        }
        sb.append("</table>")
      }
    }

    sb.toString
  }

  private def mkImgTag(img: Image, aliases: Map[String, String], relativize: String => String, firstImage: Boolean) = {
    val (cl, src) = img match {
      case i if i.mods == "main" && i.align == ">" => ("fr",   resolver.bigThumbnail(img, true))
      case i if i.mods == "main" =>                   ("main", resolver.bigThumbnail(img, false))
      case i if i.align == ">" =>                     ("thr",  resolver.thumbnail(img))
      case i =>                                       ("thz",  resolver.thumbnail(img))
    }
    val href = relativize(img.url)
    val desc = {
      val title   = if (img.title != null) mkParagraph(img.title, aliases, relativize).trim else ""
      val tags    = img.tags.visible.map { t => "<a href="+html.quoteAttribute(relativize(resolver.tag(t)))+">#"+t.title+"</a>" }.mkString(" ")
      val source  = if (img.source != null) "(<a href="+html.quoteAttribute(img.source)+">via</a>)" else ""
      val license = if (img.license != null) img.license+" "+source.trim else ""
      val locSrc  = if (img.localSource != null) "<a href="+html.quoteAttribute(relativize(resolver.link(img.localSource.slug)))+">"+html.escape(title)+"</a>" else ""
      Seq(title, tags, license, locSrc).mkString(" ").replaceAll(" +", " ").trim
    }

    val _lzy = if (!firstImage && img.mods == "main") " loading=lazy" else ""
    val _ttl = if (img.alt != null) " title="+html.quoteAttribute(img.alt) else ""
    val _src = " src="+html.quoteAttribute(relativize(src))
    val imgTag = s"""<img class=thz${_lzy}${_ttl}${_src}>"""
    val a      = if (!img.zoomable) imgTag else "<a href="+html.quoteAttribute(href)+">"+imgTag+"</a>"

    s"""<span class=$cl>$a$desc</span>"""
  }

}


sealed trait Segment
sealed trait Textual extends Segment { def txt: String }
final case class Heading(level: Int, txt: String, mods: Mods) extends Segment with Textual
final case class Paragraph(txt: String, mods: Mods = Mods()) extends Segment with Textual
final case class Inline(txt: String) extends Segment with Textual
final case class ByLine(txt: String) extends Segment with Textual
final case class Hr() extends Segment
final case class Linkref(linkMap: Seq[(String, String)]) extends Segment
final case class Images(images: Seq[Image]) extends Segment
final case class Block(tpe: String, txt: String, mods: Mods = Mods()) extends Segment
final case class Code(language: String, txt: String, mods: Mods = Mods()) extends Segment
final case class Blockquote(segments: Seq[Segment]) extends Segment
final case class SegmentSeq(segments: Seq[Segment]) extends Segment
final case class BulletList(items: Seq[Segment]) extends Segment
final case class NumberedList(items: Seq[(Int, Segment)]) extends Segment
final case class DefinitionList(items: Seq[(Inline, Seq[Segment])]) extends Segment
final case class Table(rows: Seq[Seq[Cell]]) extends Segment

case class Cell(txt: String, cols: Int, rows: Int, th: Boolean)


class AsciiMarkup(typography: Typography) extends Markup {
  val parser = new MarkupParser(typography)
  val highlighter = DefaultHighlighters // TODO

  def this() = this(NoTypography)

  def empty: AsciiText = AsciiText(Seq(), parser, highlighter, null)

  def process(lines: Seq[String], imageRoot: String): AsciiText = {
    def matchAllLines[T](ls: Seq[String], prefix: String)(f: PartialFunction[String, T]): Option[Seq[T]] = {
      if (!ls.forall(_.startsWith(prefix))) return None
      val ms = ls.collect(f)
      if (ms.size == ls.size) Some(ms) else None
    }

    def splitBlocks(lines: Seq[String]): Seq[Segment] =
      util.splitByRepeating(lines, "") map (ls => (identifySegment(ls)))

    def identifySegment(ls: Seq[String]): Segment = {
      if (ls.length == 1 && hrRegex.pattern.matcher(ls(0)).matches())
        return Hr()

      if (ls.length == 2 && headingRegex.pattern.matcher(ls(1)).matches()) {
        val rawLine = ls(0)
        val (line, mods) = findMods(rawLine) match {
          case Some((mods, pos)) => (rawLine.substring(0, pos).trim, mods)
          case None =>              (rawLine, Mods())
        }
        val level = Map('#' -> 1, '*' -> 1, '=' -> 2, '-' -> 3)(ls(1).charAt(0))
        return Heading(level, line, mods)
      }

      if (ls.length == 1 && ls(0).startsWith("---"))
        return ByLine(ls.mkString("\n"))

      if (ls(0).startsWith("-") && !(ls.length == 1 && ls(0) == "--"))
        return mkBulletList(ls)

      if (ls(0).charAt(0).isDigit && ls(0).matches("""^\d+\).*"""))
        return mkNumberedList(ls)

      if (ls(0).endsWith(":") && ls.length > 1) {
        def parseLine(l: String) = l match {
          case definitionListRegex(t) => (0, t)
          case l if l.endsWith(":")   => (1, l.init)
          case _ => null
        }

        def loop(xs: Seq[(Int, String)]): List[(Inline, Seq[Inline])] = {
          val dt = Inline(xs.head._2)
          val (dds, rest) = xs.tail.span(_._1 == 0)
          (dt, dds.map(p => Inline(p._2))) :: (if (rest.isEmpty) Nil else loop(rest))
        }

        val xs = ls.map(parseLine)
        if (xs.forall(_ != null) && xs.exists(_._1 == 0)) {
          return DefinitionList(loop(xs).toVector)
        }
      }

      None.orElse {
        matchAllLines(ls, "[*")(mkImage(imageRoot)).map(Images)

      }.orElse {
        matchAllLines(ls, "[") {
          case linkRefRegex(r, url) => (r, url)
        }.map(Linkref)

      }.orElse {
        matchAllLines(ls, ">") {
          case l if l.startsWith("> ") || l == ">"  => l.drop(2)
        }.map { ls => Blockquote(splitBlocks(ls)) }

      }.orElse {
        matchAllLines(ls, "|") {
          case l => l
        }.map(mkTable)

      }.getOrElse {
        val firstLine = ls.head
        val lastLine  = ls.last

        val (lines, mods) = findMods(firstLine) match {
          case Some((mods, pos)) =>   (firstLine.substring(0, pos) +: ls.tail, mods)
          case None => findMods(lastLine) match {
            case Some((mods, pos)) => (ls.init :+ lastLine.substring(0, pos), mods)
            case None =>              (ls, Mods())
          }
        }

        Paragraph(lines.mkString("\n"), mods)
      }
    }

    def isIndented(txt: String) = txt.linesIterator.forall(_.startsWith("  "))
    def unindent(txt: String) = txt.linesIterator.map(_.drop(2)).toVector

    def mergeParagraphsIntoLists(segments: Seq[Segment]): Seq[Segment] =
      segments.foldLeft(Vector[Segment]()) { (res, s) =>
        (s, res.lastOption) match {
          case (Paragraph(txt, _), Some(BulletList(items))) if isIndented(txt) =>
            val newSeg = identifySegment(unindent(txt))
            res.init :+ (items.last match {
              case SegmentSeq(ss) => BulletList(items.init :+ SegmentSeq(ss :+ newSeg))
              case last           => BulletList(items.init :+ SegmentSeq(Seq(last, newSeg)))
            })

          case (Paragraph(txt, _), Some(NumberedList(items))) if isIndented(txt) =>
            val newSeg = identifySegment(unindent(txt))
            res.init :+ (items.last match {
              case (i, SegmentSeq(ss)) => NumberedList(items.init :+ (i, SegmentSeq(ss :+ newSeg)))
              case (i, last)           => NumberedList(items.init :+ ((i, SegmentSeq(Seq(last, newSeg)))))
            })

          case (s, _) => res :+ s
        }
      }

    def joinNeighboringLists(segments: Seq[Segment]): Seq[Segment] =
      segments.foldLeft(Vector[Segment]()) { (res, s) =>
        (res.lastOption, s) match {
          case (Some(BulletList(items1)), BulletList(items2)) =>
            res.init :+ BulletList(items1 ++ items2)
          case (Some(NumberedList(items1)), NumberedList(items2)) =>
            res.init :+ NumberedList(items1 ++ items2)
          case (_, s) => res :+ s
        }
      }


    val ls = mutable.ArrayBuffer[String]()
    var emit = true

    for (ll <- lines) {
      val l = if (ll.contains("<!--")) commentRegex.replaceAllIn(ll, "") else ll
      val close = l.indexOf("-->")
      val open  = l.indexOf("<!--")
      if (close != -1) { emit = true }
      if (emit) {
        if (close == -1 && open == -1) ls += l
        else ls += l.substring(if (close == -1) 0 else close+3, if (open == -1) l.length else open)
      }
      if (open != -1) { emit = false }
    }

    def mkBlockBody(lines: Seq[String]) = lines.drop(1).dropRight(1).mkString("\n")+"\n"
    def mkBlockMods(mods: String) = findMods(mods).map(_._1).getOrElse(Mods())

    val segments: Seq[Segment] =
      util.splitByInterval[String](ls, (_: String).startsWith("/---"), (_: String).startsWith("\\---"))
        .flatMap { ls =>
          if (ls.head.startsWith("/---code")) {
            val Array(lang, mods) = ls.head.drop(8).trim.split(" ", 2).padTo(2, "")
            Seq(Code(lang, mkBlockBody(ls), mkBlockMods(mods)))
          } else if (ls.head.startsWith("/---")) {
            val Array(tpe, mods) = ls.head.drop(4).split(" ", 2).padTo(2, "")
            Seq(Block(tpe, mkBlockBody(ls), mkBlockMods(mods)))
          } else {
            splitBlocks(ls)
          }
        }.toVector

    AsciiText(joinNeighboringLists(mergeParagraphsIntoLists(segments)), parser, highlighter)
  }

  // image format:
  // [* image.jpg 100x100 .[class](alt) *]:[link] *** title (CC by-nc-sa https://example.com/source)

  private val imgRegex = """(?xm)
  \[\* \s+
  (\S++)
  (?: \s+ \d++x\d++ )?
  (?: \s+ \.  (?: \[ ([^\]]++) \]  (?: \( (.+?) \) )?  |  \( (.+? )  \)  )  )?
  \s+ (\*|\>|\<) \]
  (?:  :  \[ ([^\]]++) \]  )?
  (?:  \s+  (?: \*\*\* \s+ (.++))  )?
  """.r
  private val imgLicenseRegex = """(?x) (.*?) \s* (?: \( (?:(CC\ [\w-]+)\s+)? \s* ([^\ ]*)? \) )? \s*$""".r

  def mkImage(imageRoot: String): PartialFunction[String, Image] = {
    case imgRegex(url, mod, alt1, alt2, align, link, rawTitle) =>
      val u = if (link != null) link else url // ???
      val (t, license, source) = rawTitle match {
        case imgLicenseRegex(t, l, s) if licenses.contains(l) => (t, l, s)
        case imgLicenseRegex(t, null, s) => (t, null, s)
        case null => ("", null, null)
        case t    => (t,  null, null)
      }
      val (title, tags) = peelOffTags(t)
      Image(
        url = if (isAbsolute(u)) u else imageRoot + u,
        alt  = if (alt1 != null) alt1 else alt2,
        mods = mod,
        align = if (align == "*") null else align,
        title = title,
        license = license,
        source = source,
        tags = tags
      )
  }

  private val tableRowRegex = """(\|+)([^|]*)""".r
  private val tableHeadingRegex = """\|-+""".r

  private def mkTable(lines: Seq[String]): Table =
    Table(lines.foldLeft(Seq[Seq[Cell]]()) { case (rows, line) =>
      line match {
        case tableHeadingRegex() => // make cells of previous row <th>
          if (rows.isEmpty) rows else rows.init :+ rows.last.map { _.copy(th = true) }
        case line =>
          val row = tableRowRegex.findAllMatchIn(line).zipWithIndex.map { case (m, idx) =>
            val cols = m.group(1).length
            val txt  = m.group(2)
            val rows = if (!txt.endsWith("^")) 1 else -1 // mark cell to be merged later
            Cell(txt.stripPrefix("*").stripSuffix("^"), cols, rows, txt.startsWith("*"))
          }.toVector

          if (rows.isEmpty) {
            rows :+ row

          } else {
            val (prevRow, thisRow) = rows.last.zip(row).map { case (prevCell, thisCell) =>
              if (thisCell.rows == -1) (prevCell.copy(txt = prevCell.txt + thisCell.txt, rows = prevCell.rows + 1), null)
              else (prevCell, thisCell)
            }.unzip
            rows.init :+ prevRow :+ thisRow.filter(_ != null)
          }
      }
    })

  private def mkBulletList(lines: Seq[String]): BulletList = {
    val starts = (0 until lines.length).filter(i => lines(i).startsWith("-"))
    val items = (0 until starts.length)
      .map(i => lines.slice(starts(i), starts.lift(i+1).getOrElse(lines.length)))
      .map(ls => Inline(ls.map(l => l.stripPrefix("-").trim).mkString("\n")))

    BulletList(items)
  }

  private def mkNumberedList(lines: Seq[String]): NumberedList = {
    val starts = (0 until lines.length).filter(i => lines(i).matches("""^\d+\).*"""))
    val items = (0 until starts.length)
      .map(i => lines.slice(starts(i), starts.lift(i+1).getOrElse(lines.length)))
      .map { ls =>
        val Array(num, _) = ls(0).split("\\)", 2)
        num.toInt -> Inline(ls.map(l => l.replaceFirst("""^\d+\)""", "").trim).mkString("\n"))
      }

    NumberedList(items)
  }

}




// HTML markup

class HTMLMarkup extends Markup {
  val ahrefRegex  = """(?x) (?<= \<a   [^>]* href=") (.*?) (?=") """.r
  val imgsrcRegex = """(?x) (?<= \<img [^>]* src=")  (.*?) (?=") """.r

  def process(text: Seq[String], imageRoot: String): HTMLText =
    new HTMLText(text.mkString("\n"), imageRoot, this)

  def empty = HTMLText("", null, this)
}

case class HTMLText(text: String, imageRoot: String, markup: HTMLMarkup, resolver: LinkResolver = ???) extends Text {
  def render(relativize: String => String): String =
    markup.ahrefRegex.replaceAllIn(text, m => relativize(resolver.link(m.group(0))))
  def plaintextSummary: String = ""
  def plaintext: String = ???
  def images: Seq[Image] = markup.imgsrcRegex.findAllIn(text).toVector
    .map(url => Image(if (isAbsolute(url)) url else imageRoot + url))
  def links: Seq[String] = markup.ahrefRegex.findAllIn(text).map(resolver.link).toVector
  def resolve(r: LinkResolver): HTMLText = copy(resolver = r)
  def appendImages(images: Seq[Image]): Text = ???
  def mapImages(f: Image => Image): Text = ???
}
