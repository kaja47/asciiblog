package asciiblog

import MakeFiles. { Blog, licenses, peelOffTags, isAbsolute }
import AsciiMarkup._
import scala.util.matching.Regex


trait Markup {
  type ResolveLinkFunc = (String, Map[String, String]) => String

  def process(a: Article, resolveLink: ResolveLinkFunc): Text

}

trait Text {
  def render(l: ImageLayout): String
  def firstParagraph: String
  def paragraph(text: String): String

  def images: Seq[Image]
  // must return absolute urls
  def links: Seq[String]

  // Outside Markup and Text this is intended only to expand `meta: rel: [ x ]`
  // that is specified by local alias, not a valid slug.
  def linkAliases: Map[String, String]
}


// ASCII markup

object AsciiText {
  val linkRegex   = """(?x)  " ([^"]+?) " : \[ ([^\]\n]+?) \]""".r
  val ahrefRegex  = """(?x) (?<= href=") (.*?) (?=") """.r
  val imgsrcRegex = """(?x) (?<= src=") (.*?) (?=") """.r

  val linkCheck = "\":["
  val ahrefCheck = "href=\""
  val imgsrcCheck = "src=\""

  def empty = AsciiText(Seq(), null)
}

case class AsciiText(segments: Seq[Segment], resolveLink: ResolveLinkFunc) extends Text {
  import AsciiText._
  import AsciiMarkup.{ commentRegex, commentCheck }

  def render(l: ImageLayout): String = mkText(l, resolvedLinks)
  def firstParagraph: String = mkParagraph(segments.collect { case Paragraph(txt) => txt }.headOption.getOrElse(""), resolvedLinks)
  def paragraph(text: String): String = AsciiText(Seq(Inline(text)), (l, _) => resolvedLinks(l)).render(null)

  val images: Seq[Image] = segments.collect { case Images(imgs) => imgs }.flatten

  // this is before links method because links uses this one
  val linkAliases: Map[String, String] = segments.collect { case Linkref(lm) => lm }.flatMap { _.iterator }.toMap

  lazy val resolvedLinks = _links.map { l => (l, resolveLink(l, linkAliases)) }.toMap
  lazy val links: Seq[String] = resolvedLinks.valuesIterator.filter(isAbsolute).toVector

  private def _links: Seq[String] = segments.flatMap {
    case Paragraph(txt) => extractLinks(txt)
    case Inline(txt)    => extractLinks(txt)
    case Heading(txt)   => extractLinks(txt)
    case Images(images) => images.iterator.flatMap(i => extractLinks(i.title))
    case Blockquote(txt)=> txt._links
    case _ => Iterator()
  }

  private def extractLinks(txt: String): Iterator[String] =
    (if (txt.contains(ahrefCheck)) ahrefRegex.findAllMatchIn(txt).map(_.group(1)) else Iterator()) ++
    (if (txt.contains(linkCheck))  linkRegex.findAllMatchIn(txt).map(_.group(2))  else Iterator())

  private val codeRegex     = """(?xs) `    (.+?) `    """.r
  private val boldRegex     = """(?xs) \*\* (.+?) \*\* """.r
  private val italicRegex   = """(?xsUu) (?<!\*) \* (?!\*)    (.+?) (?<!\*)           \*  (?!\*) """.r
  private val italic2Regex  = """(?xsUu) (?<!:)  // (?=\b|\S) (.+?) (?<!:) (?<=\b|\S) //         """.r
  private val altRegex      = """(?xm) " ([^"]*?) \s+ \.\(  (.*?)  \)" """.r
  private val emRegex       = """---""".r
  private val blackoutRegex = """(?xs) \[\|.+?\|\] """.r
  private val list1Regex    = """(?xm) ^(-\ |\ \ )(?!\ )(.*?)(?=\n(?:-\ |\n\n|\d+\)\ )) """.r
  private val list2Regex    = """(?xm) ^(\d+\)\ )(?!\ )(.*?)(?=\n(?:\d+\)\ |\n\n)) """.r

  private val codeCheck     = """`"""
  private val boldCheck     = """**"""
  private val italicCheck   = """*"""
  private val italic2Check  = """//"""
  private val altCheck      = """.("""
  private val emCheck       = """---"""
  private val blackoutCheck = """[|"""

  private def mkParagraph(_txt: String, aliases: Map[String, String]): String = {
    var txt = _txt
    if (txt.contains(blackoutCheck)) {
      txt = blackoutRegex.replaceAllIn(txt, m => ("█"*(m.group(0).length-2)).grouped(7).mkString("<wbr>"))
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
    txt = list1Regex   .replaceAllIn(txt, "$1$2<br/>")
    txt = list2Regex   .replaceAllIn(txt, "$1$2<br/>")
    if (txt.contains(commentCheck)) {
      txt = commentRegex .replaceAllIn(txt, "")
    }
    if (txt.contains(codeCheck)) {
      txt = codeRegex    .replaceAllIn(txt, """<code>$1</code>""")
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
    txt
  }


  private def mkText(l: ImageLayout, aliases: Map[String, String]): String =
    segments.map {
      case Heading(txt)   => "<h3>"+mkParagraph(txt, aliases)+"</h3>"
      case Hr()           => "<hr/>"
      case Linkref(txt)   => ""
      case Block("html", txt) => txt
      case Block("div",  txt) => s"<div>$txt</div>"
      case Block("code", txt) => s"<pre>$txt</pre>"
      case Block("pre",  txt) => s"<pre>$txt</pre>"
      case Block("comment",_) => ""
      case Block(tpe, _)  => sys.error(s"unknown block type $tpe")
      case Images(images) => images.map(img => l.imgTag(img, this)).mkString(" ")
      case Paragraph(txt) => "<p>"+mkParagraph(txt, aliases)+"</p>"
      case Blockquote(txt) => "<blockquote>"+txt.mkText(l, aliases)+"</blockquote>"
      case Inline(txt) => mkParagraph(txt, aliases)
    }.mkString("")


}


sealed trait Segment
final case class Heading(txt: String) extends Segment
final case class Hr() extends Segment
final case class Linkref(linkMap: Map[String, String]) extends Segment
final case class Images(images: Seq[Image]) extends Segment
final case class Paragraph(txt: String) extends Segment
final case class Block(tpe: String, txt: String) extends Segment
final case class Blockquote(txt: AsciiText) extends Segment
final case class Inline(txt: String) extends Segment


object AsciiMarkup extends Markup {
  def process(a: Article, resolveLink: ResolveLinkFunc): AsciiText = segmentText(a.rawText, resolveLink)

  private val linkRefRegex  = """(?xm) ^\[(.*?)\]:\ (.+)$""".r
  private val headingRegex  = """(?xm) ^ ([^\n]+) \n ---+""".r
  private val hrRegex       = """(?xm) ---+|\*\*\*+ """.r
  private val blockRegex    = """(?xs) /---(\w+)[^\n]*\n (.*?) \\--- """.r
  val commentRegex          = """(?xs) \<!--.*?--\>""".r

  private val blockCheck    = """/---"""
  val commentCheck          = """<!--"""

  private def segmentText(_txt: String, resolveLink: ResolveLinkFunc): AsciiText = {
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
          matchAllLines(ls) {
            case linkRefRegex(r, url) => (r, url)
          }.map(refs => Linkref(refs.toMap)).orElse {
            matchAllLines(ls)(mkImage).map(Images)
          }.orElse {
            matchAllLines(ls) {
              case l if l.startsWith("> ") || l == ">"  => l.drop(2)
            }.map { ls => Blockquote(AsciiText(splitBlocks(ls.mkString("\n")), resolveLink)) }
          }.getOrElse {
            Paragraph(txt)
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

    AsciiText(segments, resolveLink)
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

  private def mkImage: PartialFunction[String, Image] = {
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
        url = if (isAbsolute(u)) u else Blog.imageRoot + u,
        alt  = if (alt1 != null) alt1 else alt2,
        mods = mod,
        align = if (align == "*") null else align,
        title = title,
        license = license,
        source = source,
        tags = tags
      )
  }

}
