package asciiblog

import java.lang.StringBuilder
import scala.util.matching.Regex
import scala.collection.mutable
import MakeFiles.{ licenses, peelOffTags, isAbsolute }
import MarkupParser.{ findMods, mkMods }
import AsciiPatterns._


trait Markup {
  type ResolveLinkFunc = String => String
  def process(text: Seq[String], resolver: ResolveLinkFunc, imageRoot: String): Text
}

trait Text {
  def render(l: ImageLayout, relativize: String => String): String
  def paragraph(text: String): String
  def plaintextSummary: String
  def plaintext: String
  def images: Seq[Image]
  def links: Seq[String]
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
  val commentRegex = """(?xs) \<!--.*?--\>""".r

  val linkUntil = Slurp { _.until('"') }
  val linkSlurp = Slurp { _.groupStart(0).quoted('"').asGroup(1, 1, -1).char(':').ignore().delimited('[', ']').asGroup(2, 1, -1).groupEnd(0) }

  def linkSlurpIterator(txt: String) =
    Slurp(txt, groups = 3).iterator(linkUntil, linkSlurp).map(_.group(2).asString())
  def linkSlurpReplace(txt: String)(f: Slurp.Replacement) =
    Slurp(txt, groups = 3).replace(linkUntil, linkSlurp, f)

  private val stripTagRegex = """\<.*?\>""".r // TODO less crude way to strip tags

  def stripTags(html: String) = T.t{
    var t = html
    t = (if (html.indexOf('<') == -1) html else stripTagRegex.replaceAllIn(html, ""))
    t = t.replace("\u00AD", "")  // soft hyphen
    t = t.replace("\u00A0", " ") // nbsp
    t = t.replace("&shy;",  "")  // soft hyphen
    t = t.replace("&nbsp;", " ") // nbsp
    t = t.replaceAll("\\s+", " ")
    t
  }
}


object AsciiText {
  def empty = AsciiText(Seq(), null, null)
}



case class AsciiText(segments: Seq[Segment], resolver: String => String, markup: AsciiMarkup) extends Text { self =>

  def render(l: ImageLayout, relativize: String => String): String = mkText(segments, l, resolvedLinks, relativize)
  def paragraph(text: String): String = AsciiText(Seq(Inline(text)), l => resolvedLinks(l), markup).render(null, identity)

  def plaintextSummary: String = stripTags(mkParagraph(segments.collect { case Paragraph(txt, _) => txt }.headOption.getOrElse(""), resolvedLinks, identity, true))
  def plaintext: String = stripTags(processTexts(segments, txt => Iterator(mkParagraph(txt, resolvedLinks, identity, true))).mkString(" "))

  // Overwrites segments, doesn't resolve links again. This saves some work but
  // mainly it's there se error messages during link resolution are not
  // displayed twice
  def overwriteSegments(newSegments: Seq[Segment]) =
    new AsciiText(newSegments, null, markup) {
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
    markup.parser(txt, (link: String) => {
      val al = aliases(link)
      if (al == Blog.invalidLinkMarker) null else relativize(al)
    }, plaintext)

  def mkText(segments: Seq[Segment], l: ImageLayout, aliases: Map[String, String], relativize: String => String): String =
    _mkText(segments, l, aliases, relativize, new StringBuilder(1024))

  private def _mkText(segments: Seq[Segment], l: ImageLayout, aliases: Map[String, String], relativize: String => String, sb: StringBuilder): String = {
    segments.foreach {
      case Heading(txt)         => sb.append("<h3>").append(mkParagraph(txt, aliases, relativize)).append("</h3>")
      case Hr()                 => sb.append("<hr/>\n")
      case Linkref(_)           =>
      case Block("html", txt, mods) => sb.append(txt) // TODO mods
      case Block("div",  txt, mods) => sb.append("<div").append(mkMods(mods)).append(">").append(txt).append("</div>")
      case Block("code", txt, mods) =>
        sb.append("<pre").append(mkMods(mods)).append(">")

        val r = "brush:(\\w+)".r
        r.findFirstMatchIn(mods.classes) match {
          case Some(m) =>
            sb.append(Highlighter.highlight(txt, m.group(1)))
          case None =>
            sb.append(util.escape(txt))
        }

        sb.append("</pre>")

      case Block("pre",  txt, mods) => sb.append("<pre").append(mkMods(mods)).append(">").append(util.escape(txt)).append("</pre>")
      case Block("comment", _, _)   =>
      case Block(tpe, _, _)        => sys.error(s"unknown block type '$tpe'")
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

      case Table(rows, _) =>
        sb.append("<table>")
        rows.foreach { cols =>
          sb.append("<tr>")
          cols.foreach { case Cell(txt, span) =>
            sb.append("<td")
            if (span > 1) {
              sb.append(" colspan=").append(span)
            }
            sb.append(">").append(mkParagraph(txt, aliases, relativize)).append("</td>")
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
final case class Block(tpe: String, txt: String, mods: Mods = Mods()) extends Segment
final case class Blockquote(segments: Seq[Segment]) extends Segment
final case class SegmentSeq(segments: Seq[Segment]) extends Segment
final case class BulletList(items: Seq[Segment]) extends Segment
final case class NumberedList(items: Seq[(Int, Segment)]) extends Segment
final case class Table(rows: Seq[Seq[Cell]], columns: Int) extends Segment // TODO columns field is not used

case class Cell(txt: String, span: Int = 1)


class AsciiMarkup extends Markup {
  val parser = new MarkupParser(CzechTypography)

  def process(lines: Seq[String], resolver: ResolveLinkFunc, imageRoot: String): AsciiText = {
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
          case (Paragraph(txt, _), Some(BulletList(items))) if txt.linesIterator.forall(_.startsWith("  ")) =>
            items.last match {
              case SegmentSeq(ss) =>
                res.init :+ BulletList(items.init :+ SegmentSeq(ss :+ s))
              case last =>
                res.init :+ BulletList(items.init :+ SegmentSeq(Seq(last, s)))
            }

          case (Paragraph(txt, _), Some(NumberedList(items))) if txt.linesIterator.forall(_.startsWith("  ")) =>
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
            val Array(tpe, mods) = ls.head.drop(4).split(" ", 2).padTo(2, "")
            Seq(Block(
              tpe,
              ls.drop(1).dropRight(1).mkString("\n")+"\n",
              findMods(mods).map(_._1).getOrElse(Mods())
            ))
          } else {
            splitBlocks(ls)
          }
        }.toVector

    val finalSegments = joinNeighboringLists(mergeParagraphsIntoLists(segments))
    AsciiText(finalSegments, resolver, this)
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

  private def mkTable(lines: Seq[String]): Table = {
    val ls = lines.filterNot { _.matches("""\|-+""" )}
    val rows = ls.map { l => tableRowRegex.findAllMatchIn(l).map { m =>
      val span = m.group(1).length
      val txt  = m.group(2)
      Cell(txt, span)
    }.toVector }
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




// HTML markup

class HTMLMarkup extends Markup {
  val ahrefRegex  = """(?x) (?<= \<a   [^>]* href=") (.*?) (?=") """.r
  val imgsrcRegex = """(?x) (?<= \<img [^>]* src=")  (.*?) (?=") """.r

  def process(text: Seq[String], resolver: String => String, imageRoot: String): HTMLText =
    new HTMLText(text.mkString("\n"), resolver, imageRoot, this)
}

class HTMLText(text: String, resolver: String => String, imageRoot: String, markup: HTMLMarkup) extends Text {
  def render(l: ImageLayout, relativize: String => String): String =
    markup.ahrefRegex.replaceAllIn(text, m => relativize(resolver(m.group(0))))
  def paragraph(text: String): String = text
  def plaintextSummary: String = ""
  def plaintext: String = ???
  def images: Seq[Image] = markup.imgsrcRegex.findAllIn(text).toVector
    .map(url => Image(if (isAbsolute(url)) url else imageRoot + url))
  def links: Seq[String] = markup.ahrefRegex.findAllIn(text).map(resolver).toVector
}
