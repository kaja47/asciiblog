package asciiblog

import MakeFiles. { licenses, peelOffTags, isAbsolute }
import AsciiMarkup._
import scala.util.matching.Regex
import scala.collection.mutable
import java.lang.StringBuilder


trait Markup {
  type ResolveLinkFunc = String => String
  def process(text: Seq[String], resolver: ResolveLinkFunc, noteUrl: String, imageRoot: String): Text
}

trait Text {
  def render(l: ImageLayout, relativize: String => String): String
  def firstParagraph: String
  def paragraph(text: String): String
  def images: Seq[Image]
  def links: Seq[String]
}


// ASCII markup

object AsciiText {
  val linkRegex   = """(?x)  " ([^"]+?) " : \[ ([^\]\n]+?) \]""".r
  val ahrefRegex  = """(?x) (?<= href=") (.*?) (?=") """.r
  val imgsrcRegex = """(?x) (?<= src=") (.*?) (?=") """.r

  val linkCheck = "\":["
  val ahrefCheck = "href=\""
  val imgsrcCheck = "src=\""

  val linkUntil = Slurp { _.until('"') }
  val linkSlurp = Slurp { _.groupStart(0).quoted('"').asGroup(1, 1, -1).char(':').ignore().delimited('[', ']').asGroup(2, 1, -1).groupEnd(0) }

  def linkSlurpIterator(txt: String) =
    Slurp(txt, groups = 3).iterator(linkUntil, linkSlurp).map(_.group(2).asString())
  def linkSlurpReplace(txt: String)(f: Slurp.Replacement) =
    Slurp(txt, groups = 3).replace(linkUntil, linkSlurp, f)

  def empty = AsciiText(Seq(), null, "")
}

case class AsciiText(segments: Seq[Segment], resolver: ResolveLinkFunc, noteUrl: String) extends Text { self =>
  import AsciiText._

  def render(l: ImageLayout, relativize: String => String): String = mkText(segments, l, resolvedLinks, relativize)
  def firstParagraph: String = mkParagraph(segments.collect { case Paragraph(txt, _) => txt }.headOption.getOrElse(""), resolvedLinks, identity, true)
  def paragraph(text: String): String = AsciiText(Seq(Inline(text)), l => resolvedLinks(l), "").render(null, identity)

  // Overwrites segments, doesn't resolve links again. This saves some work but
  // mainly it's there se error messages during link resolution are not
  // displayed twice
  def overwriteSegments(newSegments: Seq[Segment]) =
    new AsciiText(newSegments, null, noteUrl) {
      override protected def resolvedLinks = self.resolvedLinks
    }

  val images: Seq[Image] = segments.collect { case Images(imgs) => imgs }.flatten

  private def processTexts[T](segments: Seq[Segment], f: String => Iterator[T]): Iterator[T] = segments.iterator.flatMap {
    case t: Textual     => f(t.txt)
    case Images(images) => images.iterator.flatMap(i => f(i.title))
    case Blockquote(sx) => processTexts(sx, f)
    case SegmentSeq(sx) => processTexts(sx, f)
    case BulletList(items)   => processTexts(items, f)
    case NumberedList(items) => processTexts(items.map(_._2), f)
    case Table(rows, _) => rows.iterator.flatten.flatMap(cell => f(cell.txt))
    case _ => Iterator()
  }

  private def extractLinks(txt: String): Iterator[String] = // 100 ms
    (if (txt.contains(ahrefCheck)) ahrefRegex.findAllMatchIn(txt).map(_.group(1)) else Iterator()) ++
    (if (txt.contains(linkCheck))  linkSlurpIterator(txt)                         else Iterator())

  //require(
  //  processTexts(segments, txt => linkRegex.findAllMatchIn(txt).map(_.group(2))).toVector ==
  //  processTexts(segments, linkSlurpIterator).toVector
  //)

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
      (l, resolver(aliasMap.getOrElse(base, base)+hash))
    }.toMap
  }

  lazy val links: Seq[String] = resolvedLinks.valuesIterator.toVector

  private lazy val validRefTargets: Map[Int, NumberedList] = {
    val refLinks = processTexts(segments, txt => noteRegex.findAllMatchIn(txt).map(_.group(1).toInt)).toSet

    if (noteUrl.isEmpty) {
      val refTargets = segments.collect { case NumberedList(items) => items.map(_._1) }.flatten.toSet
      refLinks.foreach { num =>
        if (!refTargets.contains(num)) sys.error(s"invalid reference $num")
      }
    }

    segments.flatMap {
      case list @ NumberedList(items) =>
        items.map { case (num, _) => (num, list) }
      case _ => Seq()
    }.toMap // last key should be used
  }

  private def mkParagraph(_txt: String, aliases: Map[String, String], relativize: String => String, plaintext: Boolean = false): String = {
    var txt = _txt
    if (txt.contains(blackoutCheck)) {
      txt = blackoutRegex.replaceAllIn(txt, m => {
          val sb = new StringBuilder()
          val len = m.end - m.start - 2
          for (i <- 0 to len/5) { sb.append("█████").append("<wbr>") }
          sb.delete(sb.length-5, sb.length).toString
      })
    }
    if (txt.contains(altCheck)) {
      txt = altRegex.replaceAllIn(txt, if (plaintext) "$1" else """<span class=about title="$2">$1</span>""")
    }
    if (txt.contains(ahrefCheck)) {
      txt = ahrefRegex.replaceAllIn(txt, m => Regex.quoteReplacement(relativize(aliases(m.group(1)))))
    }
    if (txt.contains(linkCheck)) {
      txt = linkSlurpReplace(txt) { (s, sb) =>
        if (plaintext) {
          sb append s.group(1).asString()
        } else {
          val link = aliases(s.group(2).asString())
          if (link == Blog.invalidLinkMarker) {
            sb append s.group(1).asString()
          } else {
            val l = relativize(link)
            sb append "<a href=" append util.quoteHTMLAttribute(l) append ">" append s.group(1).asString() append "</a>"
          }
        }
      }.toString
    }
    if (txt.contains(commentCheck)) {
      txt = commentRegex.replaceAllIn(txt, "")
    }
    if (txt.contains(boldCheck)) {
      txt = boldRegex.replaceAllIn(txt, if (plaintext) "$1" else """<b>$1</b>""")
    }
    if (txt.contains(italicCheck)) {
      txt = italicRegex.replaceAllIn(txt, if (plaintext) "$1" else """<i>$1</i>""")
    }
    if (txt.contains(italic2Check)) {
      txt = italic2Regex.replaceAllIn(txt, if (plaintext) "$1" else  """<i>$1</i>""")
    }
    if (txt.contains(emCheck)) {
      txt = emRegex.replaceAllIn(txt, "&mdash;")
    }
    if (txt.contains(codeCheck)) {
      txt = codeRegex.replaceAllIn(txt, m => "<code>"+Regex.quoteReplacement(util.escape(m.group(1)))+"</code>")
    }
    if (txt.contains(noteCheck)) {
      txt = noteRegex.replaceAllIn(txt, m => {
        if (plaintext) "" else {
          val l = noteUrl+"#fn"+m.group(1)
          Regex.quoteReplacement(s"""<a href=${util.quoteHTMLAttribute(l)}><sup>${m.group(1)}</sup></a> """)
        }
      })
    }
    //txt = preposRegex.replaceAllIn(txt, "$1\u00A0")

    {
      if (txt.length >= 2) {
        def ws(ch: Char) = (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r')

        val res = new StringBuilder(txt.length+128)
        var pos = 0

        var i = 2; while (i < txt.length) {
          if (ws(txt.charAt(i)) && isPreposChar(txt.charAt(i-1)) && ws(txt.charAt(i-2))) {
            res.append(txt, pos, i).append("\u00A0")
            pos = i+1
          }
          i += 1
        }
        res.append(txt, pos, txt.length)
        txt = res.toString
      }
    }

    txt
  }

  def mkMods(m: Mods): String = {
    if (m.isEmpty) return "" // the most mods are always empty

    val (ids, classes) =
      if (m.classes.isEmpty) (Array[String](), Array[String]())
      else m.classes.split(" ").partition(_.startsWith("#"))

    (if (m.title.nonEmpty)  " title="+util.quoteHTMLAttribute(m.title)                       else "")+
    (if (classes.nonEmpty)  " class="+util.quoteHTMLAttribute(classes.mkString(" "))         else "")+
    (if (ids.nonEmpty)      " id="   +util.quoteHTMLAttribute(ids.map(_.tail).mkString(" ")) else "")+
    (if (m.styles.nonEmpty) " style="+util.quoteHTMLAttribute(m.styles)                      else "")
  }

  def mkText(segments: Seq[Segment], l: ImageLayout, aliases: Map[String, String], relativize: String => String): String =
    _mkText(segments, l, aliases, relativize, new StringBuilder(1024))

  private def _mkText(segments: Seq[Segment], l: ImageLayout, aliases: Map[String, String], relativize: String => String, sb: StringBuilder): String = {
    segments.foreach {
      case Heading(txt)         => sb.append("<h3>").append(mkParagraph(txt, aliases, relativize)).append("</h3>")
      case Hr()                 => sb.append("<hr/>\n")
      case Linkref(_)           =>
      case Block("html", txt)   => sb.append(txt)
      case Block("div",  txt)   => sb.append("<div>").append(txt).append("</div>")
      case Block("code", txt)   => sb.append("<pre>").append(util.escape(txt)).append("</pre>")
      case Block("pre",  txt)   => sb.append("<pre>").append(util.escape(txt)).append("</pre>")
      case Block("comment",_)   =>
      case Block(tpe, _)        => sys.error(s"unknown block type '$tpe'")
      case Images(images)       => images.foreach { img => sb.append(l.imgTag(img, this)).append(" ") }
      case Paragraph(txt, mods) => sb.append("<p").append(mkMods(mods)).append(">").append(mkParagraph(txt, aliases, relativize)).append("</p>")
      case Blockquote(sx)       => sb.append("<blockquote>"); _mkText(sx, l, aliases, relativize, sb); sb.append("</blockquote>")
      case Inline(txt)          => sb.append(mkParagraph(txt, aliases, relativize))
      case ByLine(txt)          => sb.append("<div style='text-align:right'>").append(mkParagraph(txt, aliases, relativize)).append("</div>")
      case SegmentSeq(sx)       => _mkText(sx, l, aliases, relativize, sb)
      case BulletList(items)    =>
        sb.append("<ul>")
        items.foreach { it =>
          sb.append("<li>")
          _mkText(Seq(it), l, aliases, relativize, sb)
          sb.append("</li>\n")
        }
        sb.append("</ul>")

      case list @ NumberedList(items) =>
        sb.append("<ol>")
        items.zipWithIndex.foreach {
          case ((num, it), i) =>
            sb.append("<li")
            if (validRefTargets(num) == list) { sb.append(" id=").append(util.quoteHTMLAttribute("fn"+num)) }
            if (num != i+1)                   { sb.append(" value=").append(util.quoteHTMLAttribute(num.toString)) }
            sb.append(">")
            _mkText(Seq(it), l, aliases, relativize, sb)
            sb.append("</li>\n")
        }
        sb.append("</ol>")

      case Table(rows, columns) =>
        sb.append("<table>")
        rows.foreach { cols =>
          sb.append("<tr>")
          cols.foreach { cell =>
            sb.append("<td>").append(mkParagraph(cell.txt, aliases, relativize)).append("</td>")
          }
          sb.append("</tr>")
        }
        sb.append("</table>")
    }

    sb.toString
  }

}


sealed trait Segment
sealed trait Textual extends Segment { def txt: String }
final case class Heading(txt: String) extends Segment with Textual
final case class Paragraph(txt: String, mods: Mods = Mods()) extends Segment with Textual
final case class Inline(txt: String) extends Segment with Textual
final case class ByLine(txt: String) extends Segment with Textual
final case class Hr() extends Segment
final case class Linkref(linkMap: Seq[(String, String)]) extends Segment
final case class Images(images: Seq[Image]) extends Segment
final case class Block(tpe: String, txt: String) extends Segment
final case class Blockquote(segments: Seq[Segment]) extends Segment
final case class SegmentSeq(segments: Seq[Segment]) extends Segment
final case class BulletList(items: Seq[Segment]) extends Segment
final case class NumberedList(items: Seq[(Int, Segment)]) extends Segment
final case class Table(rows: Seq[Seq[Cell]], columns: Int) extends Segment

case class Cell(txt: String, span: Int = 1)

case class Mods(title: String = "", classes: String = "", styles: String = "") {
  def join(a: String, b: String, delim: String) = if (a.isEmpty || b.isEmpty) a+b else a+delim+b
  def merge(m: Mods) = Mods(join(title, m.title, " "), join(classes, m.classes, " "), join(styles, m.styles, ";"))
  def isEmpty = title.isEmpty && classes.isEmpty && styles.isEmpty
}



object AsciiMarkup extends Markup {
  def process(text: Seq[String], resolver: ResolveLinkFunc, noteUrl: String, imageRoot: String): AsciiText = segmentText(text, resolver, noteUrl, imageRoot)

  val codeRegex     = """(?xs) `    (.+?) `    """.r
  val boldRegex     = """(?xs) \*\* (.+?) \*\* """.r
  val italicRegex   = """(?xsUu) (?<!\*) \* (?!\*)    ((?:.(?!`))+?) (?<!\*)  \*  (?!\*) """.r
  val italic2Regex  = """(?xsUu) (?<!:)  // (?=\b|\S) (.+?) (?<!:) (?<=\b|\S) //         """.r
  val altRegex      = """(?xs) " ([^"]*?) \s+ \.\(  (.*?)  \)" """.r
  val emRegex       = """---""".r
  val blackoutRegex = """(?xs) \[\|.+?\|\] """.r
  val noteRegex     = """(?x) \[\[ (\d++) \]\]""".r
  val preposRegex   = """(?xuUms) ((?:^|\s|\>)[ksvzouiKSVZOUIA])\s++(?=\w)""".r // for unbreakable space between preposition and word (czech)

  val preposCharsLen = 123
  val preposChars = Array.tabulate[Boolean](preposCharsLen) { i => "ksvzouiaKSVZOUIA".indexOf(i) != -1 }
  def isPreposChar(ch: Char) = ch < preposCharsLen && preposChars(ch)

  val codeCheck     = """`"""
  val boldCheck     = """**"""
  val italicCheck   = """*"""
  val italic2Check  = """//"""
  val altCheck      = """.("""
  val emCheck       = """---"""
  val blackoutCheck = """[|"""
  val noteCheck     = """[["""

  private val linkRefRegex  = """(?xm) ^\[(.*?)\]:\ +(.+)$""".r
  private val hrRegex       = """(?xm) ---+|\*\*\*+ """.r
  private val blockRegex    = """(?xs) /---(\w+)[^\n]*\n (.*?) \\--- """.r
  val commentRegex          = """(?xs) \<!--.*?--\>""".r

  val commentCheck          = """<!--"""

  // .(title)  .[class1 class2 #id]  .{color:blue}  .<  .>  .<>  .=
  def findMods(line: String): Option[(Mods, Int)] = {
    val s = Slurp(line)
    while (s.to('.').matches && !s.touchesEnd) {
      s.ignore()
      var mods = Mods()
      val start = s.pos
      var ok = true
      while (ok && !s.touchesEnd) {
        ok = modAlternatives.exists { case (f, ex) =>
          f(s)
          if (s.matches) {
            val x = ex(s)
            s.ignore()
            mods = mods.merge(x)
            true
          } else {
            s.unmatch().tryAgain()
            false
          }
        }
      }
      if (ok) return Some((mods, start-1))
    }
    None
  }

  private val modAlternatives = Seq[(Slurp => Unit, Slurp => Mods)](
    (_.quoted('(', ')'), s => Mods(title   = s.asString(1, -1))),
    (_.quoted('[', ']'), s => Mods(classes = s.asString(1, -1))),
    (_.quoted('{', '}'), s => Mods(styles  = s.asString(1, -1))),
    (_.string("<>"),     s => Mods(styles = "text-align:center")),
    (_.string(">"),      s => Mods(styles = "text-align:right")),
    (_.string("<"),      s => Mods(styles = "text-align:left")),
    (_.string("="),      s => Mods(styles = "text-align:justify"))
  )

  private def segmentText(lines: Seq[String], resolver: ResolveLinkFunc, noteUrl: String, imageRoot: String): AsciiText = {
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

      if (ls.length == 2 && hrRegex.pattern.matcher(ls(1)).matches())
        return Heading(ls(0))

      if (ls.length == 1 && ls(0).startsWith("---"))
        return ByLine(ls.mkString("\n"))

      if (ls(0).startsWith("-") && !(ls.length == 1 && ls(0) == "--"))
        return mkBulletList(ls)

      if (ls(0).charAt(0).isDigit && ls(0).matches("""^\d+\).*"""))
        return mkNumberedList(ls)

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

    def mergeParagraphsIntoLists(segments: Seq[Segment]): Seq[Segment] = // TODO more general mechanism for not only paragraphs
      segments.foldLeft(Vector[Segment]()) { (res, s) =>
        (s, res.lastOption) match {
          case (Paragraph(txt, _), Some(BulletList(items))) if txt.lines.forall(_.startsWith("  ")) =>
            items.last match {
              case SegmentSeq(ss) =>
                res.init :+ BulletList(items.init :+ SegmentSeq(ss :+ s))
              case last =>
                res.init :+ BulletList(items.init :+ SegmentSeq(Seq(last, s)))
            }

          case (Paragraph(txt, _), Some(NumberedList(items))) if txt.lines.forall(_.startsWith("  ")) =>
            items.last match {
              case (i, SegmentSeq(ss)) =>
                res.init :+ NumberedList(items.init :+ (i, SegmentSeq(ss :+ s)))
              case (i, last) =>
                res.init :+ NumberedList(items.init :+ ((i, SegmentSeq(Seq(last, s)))))
            }

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

    val segments: Seq[Segment] =
      util.splitByInterval[String](ls, (_: String).startsWith("/---"), (_: String).startsWith("\\---"))
        .flatMap { ls =>
          if (ls.head.startsWith("/---")) {
            Seq(Block(ls.head.drop(4).split(" ").head, ls.drop(1).dropRight(1).mkString("\n")+"\n"))
          } else {
            splitBlocks(ls)
          }
        }.toVector

    val finalSegments = joinNeighboringLists(mergeParagraphsIntoLists(segments))
    AsciiText(finalSegments, resolver, noteUrl)
  }

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

  private def mkTable(lines: Seq[String]): Table = {
    val ls = lines.filterNot{ _.matches("""\|-+""" )}
    val rows = ls.map { l => l.split("\\|").toSeq.tail.map { cell => Cell(cell, 1) } }
    val columns = rows.map(_.map(_.span).sum).max
    Table(rows, columns)
  }

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
