package asciiblog

import MakeFiles. { licenses, peelOffTags, isAbsolute }
import AsciiMarkup._
import scala.util.matching.Regex


trait Markup {
  type ResolveLinkFunc = (String, Map[String, String]) => String

  def process(text: String, resolveLink: ResolveLinkFunc, noteUrl: String, imageRoot: String): Text
  def process(a: Article, resolveLink: ResolveLinkFunc, noteUrl: String, imageRoot: String): Text =
    process(a.rawText, resolveLink, noteUrl, imageRoot)

}

trait Text {
  def render(l: ImageLayout): String
  def firstParagraph: String
  def paragraph(text: String): String

  def images: Seq[Image]
  // must return absolute urls
  def links: Seq[String]

  // Outside Markup and Text this is intended only for
  // - expanding `meta: rel: [ x ]` specified by a local alias, not a valid slug
  // - check if there are not duplicate aliases
  def linkAliases: Seq[(String, String)]
}


// ASCII markup

object AsciiText {
  val linkRegex   = """(?x)  " ([^"]+?) " : \[ ([^\]\n]+?) \]""".r
  val ahrefRegex  = """(?x) (?<= href=") (.*?) (?=") """.r
  val imgsrcRegex = """(?x) (?<= src=") (.*?) (?=") """.r

  val linkCheck = "\":["
  val ahrefCheck = "href=\""
  val imgsrcCheck = "src=\""

  def empty = AsciiText(Seq(), null, "")
}

case class AsciiText(segments: Seq[Segment], resolveLink: ResolveLinkFunc, noteUrl: String) extends Text {
  import AsciiText._
  import AsciiMarkup.{ commentRegex, commentCheck }

  def render(l: ImageLayout): String = mkText(segments, l, resolvedLinks)
  def firstParagraph: String = mkParagraph(segments.collect { case Paragraph(txt) => txt }.headOption.getOrElse(""), resolvedLinks)
  def paragraph(text: String): String = AsciiText(Seq(Inline(text)), (l, _) => resolvedLinks(l), "").render(null)

  val images: Seq[Image] = segments.collect { case Images(imgs) => imgs }.flatten

  // this is before links method because links uses this one
  val linkAliases: Seq[(String, String)] = segments.collect { case Linkref(lm) => lm }.flatMap { _.iterator }
  val linkAliasesMap = linkAliases.toMap

  lazy val resolvedLinks = _links(segments).map { l => (l, resolveLink(l, linkAliasesMap)) }.toMap
  lazy val links: Seq[String] = resolvedLinks.valuesIterator.filter(isAbsolute).toVector

  private def _links(segments: Seq[Segment]): Seq[String] = segments.flatMap {
    case Paragraph(txt) => extractLinks(txt)
    case Inline(txt)    => extractLinks(txt)
    case Heading(txt)   => extractLinks(txt)
    case Images(images) => images.iterator.flatMap(i => extractLinks(i.title))
    case Blockquote(sx) => _links(sx)
    case SegmentSeq(sx) => _links(sx)
    case BulletList(items)   => _links(items)
    case NumberedList(items) => _links(items.map(_._2))
    case Table(rows, _) => rows.iterator.flatten.flatMap(cell => extractLinks(cell.txt))
    case _ => Iterator()
  }

  private def extractLinks(txt: String): Iterator[String] =
    (if (txt.contains(ahrefCheck)) ahrefRegex.findAllMatchIn(txt).map(_.group(1)) else Iterator()) ++
    (if (txt.contains(linkCheck))  linkRegex.findAllMatchIn(txt).map(_.group(2))  else Iterator())

  private val codeRegex     = """(?xs) `    (.+?) `    """.r
  private val boldRegex     = """(?xs) \*\* (.+?) \*\* """.r
  private val italicRegex   = """(?xsUu) (?<!\*) \* (?!\*)    ((?:.(?!`))+?) (?<!\*)  \*  (?!\*) """.r
  private val italic2Regex  = """(?xsUu) (?<!:)  // (?=\b|\S) (.+?) (?<!:) (?<=\b|\S) //         """.r
  private val altRegex      = """(?xm) " ([^"]*?) \s+ \.\(  (.*?)  \)" """.r
  private val emRegex       = """---""".r
  private val blackoutRegex = """(?xs) \[\|.+?\|\] """.r
  private val noteRegex     = """(?x) \[\[ (\d++) \]\]""".r
  private val preposRegex   = """(?xuUms) (?<=^|\W)([ksvzouiKSVZOUIA])\s++(?=\w)""".r // for unbreakable space between preposition and word

  private val codeCheck     = """`"""
  private val boldCheck     = """**"""
  private val italicCheck   = """*"""
  private val italic2Check  = """//"""
  private val altCheck      = """.("""
  private val emCheck       = """---"""
  private val blackoutCheck = """[|"""
  private val noteCheck     = """[["""

  private def mkParagraph(_txt: String, aliases: Map[String, String]): String = {
    var txt = _txt
    if (txt.contains(blackoutCheck)) {
      txt = blackoutRegex.replaceAllIn(txt, m => ("█"*(m.group(0).length-2)).grouped(5).mkString("<wbr>"))
    }
    if (txt.contains(altCheck)) {
      txt = altRegex     .replaceAllIn(txt, """<span class=about title="$2">$1</span>""")
    }
    if (txt.contains(ahrefCheck)) {
      txt = ahrefRegex   .replaceAllIn(txt, m => Regex.quoteReplacement(aliases(m.group(1))))
    }
    if (txt.contains(linkCheck)) {
      txt = linkRegex    .replaceAllIn(txt, m => Regex.quoteReplacement(s"""<a href="${aliases(m.group(2))}">${m.group(1)}</a>"""))
    }
    if (txt.contains(commentCheck)) {
      txt = commentRegex .replaceAllIn(txt, "")
    }
    if (txt.contains(boldCheck)) {
      txt = boldRegex    .replaceAllIn(txt, """<b>$1</b>""")
    }
    if (txt.contains(italicCheck)) {
      txt = italicRegex  .replaceAllIn(txt, """<i>$1</i>""")
    }
    if (txt.contains(italic2Check)) {
      txt = italic2Regex .replaceAllIn(txt, """<i>$1</i>""")
    }
    if (txt.contains(emCheck)) {
      txt = emRegex      .replaceAllIn(txt, "&mdash;")
    }
    if (txt.contains(codeCheck)) {
      txt = codeRegex    .replaceAllIn(txt, """<code>$1</code>""")
    }
    if (txt.contains(noteCheck)) {
      txt = noteRegex.replaceAllIn(txt, m => Regex.quoteReplacement(s"""<a href="$noteUrl#fn${m.group(1)}"><sup>${m.group(1)}</sup></a> """))
    }
    txt = preposRegex.replaceAllIn(txt, "$1\u00A0")
    txt
  }

  private def mkText(segments: Seq[Segment], l: ImageLayout, aliases: Map[String, String]): String =
    segments.map {
      case Heading(txt)         => "<h3>"+mkParagraph(txt, aliases)+"</h3>"
      case Hr()                 => "<hr/>\n"
      case Linkref(txt)         => ""
      case Block("html", txt)   => txt
      case Block("div",  txt)   => s"<div>$txt</div>"
      case Block("code", txt)   => s"<pre>$txt</pre>"
      case Block("pre",  txt)   => s"<pre>$txt</pre>"
      case Block("comment",_)   => ""
      case Block(tpe, _)        => sys.error(s"unknown block type $tpe")
      case Images(images)       => images.map(img => l.imgTag(img, this)).mkString(" ")
      case Paragraph(txt)       => "<p>"+mkParagraph(txt, aliases)+"</p>"
      case Blockquote(sx)       => "<blockquote>"+mkText(sx, l, aliases)+"</blockquote>"
      case Inline(txt)          => mkParagraph(txt, aliases)
      case SegmentSeq(sx)       => mkText(sx, l, aliases)
      case BulletList(items)    => "<ul>"+items.map{ i => "<li>"+mkText(Seq(i), l, aliases)+"</li>" }.mkString("\n")+"</ul>"
      case NumberedList(items)  => "<ol>"+items.map{ case (num, i) => "<li value="+num+" id=\"fn"+num+"\">"+mkText(Seq(i), l, aliases)+"</li>" }.mkString("\n")+"</ol>"
      case Table(rows, columns) =>
        "<table>"+
        rows.map(cols =>
          "<tr>"+
          cols.map(cell => "<td>"+mkParagraph(cell.txt, aliases)+"</td>").mkString+
          "</tr>"
        ).mkString+
        "</table>"
    }.mkString("")


}


sealed trait Segment
final case class Heading(txt: String) extends Segment
final case class Hr() extends Segment
final case class Linkref(linkMap: Seq[(String, String)]) extends Segment
final case class Images(images: Seq[Image]) extends Segment
final case class Paragraph(txt: String) extends Segment
final case class Block(tpe: String, txt: String) extends Segment
final case class Blockquote(segments: Seq[Segment]) extends Segment
final case class Inline(txt: String) extends Segment
final case class SegmentSeq(segments: Seq[Segment]) extends Segment
final case class BulletList(items: Seq[Segment]) extends Segment
final case class NumberedList(items: Seq[(Int, Segment)]) extends Segment
final case class Table(rows: Seq[Seq[Cell]], columns: Int) extends Segment

case class Cell(txt: String, span: Int = 1)


object AsciiMarkup extends Markup {
  def process(text: String, resolveLink: ResolveLinkFunc, noteUrl: String, imageRoot: String): AsciiText = segmentText(text, resolveLink, noteUrl, imageRoot)

  private val linkRefRegex  = """(?xm) ^\[(.*?)\]:\ (.+)$""".r
  private val headingRegex  = """(?xm) ^ ([^\n]+) \n ---+""".r
  private val hrRegex       = """(?xm) ---+|\*\*\*+ """.r
  private val blockRegex    = """(?xs) /---(\w+)[^\n]*\n (.*?) \\--- """.r
  val commentRegex          = """(?xs) \<!--.*?--\>""".r

  private val blockCheck    = """/---"""
  val commentCheck          = """<!--"""

  private def segmentText(_txt: String, resolveLink: ResolveLinkFunc, noteUrl: String, imageRoot: String): AsciiText = {
    def matchAllLines[T](ls: Seq[String])(f: PartialFunction[String, T]): Option[Seq[T]] = {
      val ms = ls.map(f.lift)
      if (ms.forall(_.isDefined)) Some(ms.map(_.get)) else None
    }

    def splitBlocks(txt: String): Seq[Segment] =
      txt.split("\n\n+") map {
        case headingRegex(txt) => Heading(txt)
        case hrRegex() => Hr()
        case txt =>
          val ls = txt.lines.toVector

          None.orElse {
            matchAllLines(ls) {
              case linkRefRegex(r, url) => (r, url)
            }.map(Linkref)

          }.orElse {
            matchAllLines(ls)(mkImage(imageRoot)).map(Images)

          }.orElse {
            matchAllLines(ls) {
              case l if l.startsWith("> ") || l == ">"  => l.drop(2)
            }.map { ls => Blockquote(splitBlocks(ls.mkString("\n"))) }

          }.orElse {
            matchAllLines(ls) {
              case l if l.startsWith("|") => l
            }.map(mkTable)

          }.orElse {
            if (ls(0).startsWith("-")) Some(mkBulletList(ls)) else None

          }.orElse {
            if (ls(0).matches("""^\d+\).*""")) Some(mkNumberedList(ls)) else None

          }.getOrElse {
            Paragraph(txt)
          }
      }

    def mergeParagraphsIntoLists(segments: Seq[Segment]): Seq[Segment] = { // TODO more general mechanism for not only paragraphs
      segments.foldLeft(Vector[Segment]()) { (res, s) =>
        (s, res.lastOption) match {
          case (s @ Paragraph(txt), Some(BulletList(items))) if txt.lines.forall(_.startsWith("  ")) =>
            items.last match {
              case SegmentSeq(ss) =>
                res.init :+ BulletList(items.init :+ SegmentSeq(ss :+ s))
              case last =>
                res.init :+ BulletList(items.init :+ SegmentSeq(Seq(last, s)))
            }

          case (Paragraph(txt), Some(NumberedList(items))) if txt.lines.forall(_.startsWith("  ")) =>
            items.last match {
              case (i, SegmentSeq(ss)) =>
                res.init :+ NumberedList(items.init :+ (i, SegmentSeq(ss :+ s)))
              case (i, last) =>
                res.init :+ NumberedList(items.init :+ ((i, SegmentSeq(Seq(last, s)))))
            }

          case (s, _) => res :+ s
        }
      }
    }

    var txt = commentRegex.replaceAllIn(_txt, "")

    val segments: Seq[Segment] = {
      var prev = 0
      (for (m <- blockRegex.findAllMatchIn(txt)) yield {
        val block = Block(m.group(1), m.group(2))
        val res = splitBlocks(txt.substring(prev, m.start)) :+ block
        prev = m.end
        res
      }).toVector.flatten ++ (if (prev > txt.length) Seq() else splitBlocks(txt.substring(prev)))
    }

    AsciiText(mergeParagraphsIntoLists(segments), resolveLink, noteUrl)
  }

  private val imgRegexFragment = """
  \s*  \[\*  \s+
  ([^*\n]+?) (?= \s+ \d++x\d++ | \s+ \.\[ | \s+ \.\( | \s+ (?:\*|\>|\<)\] )
  (?: \s+ \d++x\d++ )?
  (?:  \s+  \.  (?:  \[ ([^\]\n]++) \]  (?: \( ([^\n]+? ) \) )?  |  \( ([^\n]+? )  \)  )  )?
  \s+
  (\*|\>|\<)\]
  (?:  :  \[ ([^\]\n]++) \]  )?
  (?:  \s+  (?: \*\*\* \s+ ([^\n]++))  )?
  \s*
  """
  private val imgRegex      = s"""(?xm) $imgRegexFragment""".r
  private val imgLicenseRegex = """(?x) (.*?) \s* (?: \( (?:(CC\ [\w-]+)\s+)? \s* ([^\ ]*)? \) )? \s*$""".r

  private def mkImage(imageRoot: String): PartialFunction[String, Image] = {
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
