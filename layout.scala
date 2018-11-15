package asciiblog

import MakeFiles. { year, month, galleryScript }
import AsciiText. { ahrefRegex }
import java.text.SimpleDateFormat
import java.util.Date
import java.net.URLEncoder
import scala.util.matching.Regex
import util.escape


trait LayoutMill {
  def make(baseUrl: String): Layout
}

trait Layout extends ImageLayout {
  val baseUrl: String
  def makePage(content: String, title: String = null, containImages: Boolean = false, headers: String = null, includeCompleteStyle: Boolean = false, article: Option[Article] = None): String
  def makeIndex(fullArticles: Seq[Article], links: Seq[Article], archiveLinks: Seq[Article] = Seq(), groupArchiveByMonth: Boolean = false, tagsToShow: Seq[Article] = Seq()): String
  def makeFullArticle(a: Article): String
  def makeTagIndex(base: Base): String
  def addArrows(content: String, next: Article, prev: Article, includeBottom: Boolean = false): String

  def articleUrl(a: Article): String
  def articleLink(a: Article, title: String, asLink: Boolean = true, allowImageMarker: Boolean = false): String
}

// this type is passed into markup
trait ImageLayout {
  def imgTag(img: Image, t: Text, showDesc: Boolean = true, linkTo: String = null): String
}



class FlowLayoutMill(base: Base, blog: Blog, markup: Markup, val resolver: String => String) {
  def make(baseUrl: String): FlowLayout = FlowLayout(baseUrl, base, blog, markup, this)

  private def inlineStyles = s"""
a          { color: inherit; }
.r         { text-align: right; }
.f         { float: right; }
.b         { max-width: 46em; font-family: monospace; line-height: 1.3; }
blockquote { margin:0; padding:0; font-style:italic; }
.about     { text-decoration: underline red; }
.thz, .fr, .main
           { font-size: 0.8em }
span.thz   { width: ${blog.thumbWidth}px; display: inline-block; vertical-align: top; }
span.fr    { text-align: right; max-width: 45%; float: right; }
span.main  { text-align: center; display: block; margin-bottom: 0.5em; }
span.main img, span.fr img
           { max-width: 100%; }
h2         { display: inline; margin: 0; font-size:1em; }
hr         { border: 0px dotted gray; border-top-width: 1px; margin: 0.8em 4em; }
p          { margin: 1.4em 0; }
.sh        { float: left; clear: both; margin: 0.8em 0; }
.shimg     { float: left; margin: 0 0.5em 0 0; }
footer     { font-size: 0.9em; border-top: 1px dashed gray; padding-top: 1em; color: #222; }
article    { margin-bottom: 4em; }
aside      { clear:both; margin-bottom: 2em; font-size: 0.9em; }
""".trim

  import CssMinimizer._
  private val optStyle = optimize(parseCSS(inlineStyles) ++ parseCSS(blog.cssStyle))

  def style(cats: Set[String]) = cats match {
    case null => render(optStyle)
    case cats => render2(minimize(optStyle, cats))
  }

  val defaultHeader = s"""<div class=r><b><a href="index">${blog.title}</a></b> [<a href="rss.xml">RSS</a>]</div>"""
  val header = new PrepatedText(if (blog.header.nonEmpty) blog.header else defaultHeader, resolver)
  val footer = new PrepatedText(blog.footer, resolver)
}


object FlowLayout {
  val __ahrefRegex = """(?xs) (<a [^>]* href=") ([^"]+) (" [^>]* >) (.*?) (</a>)""".r

  private val stripTagRegex = """\<.*?\>""".r
  private val truncateRegex = """\s+(\w+)?$|\<\w+$""".r

  def stripTags(html: String) =
    stripTagRegex.replaceAllIn(html, "") // TODO less crude way to strip tags

  def truncate(txt: String, len: Int, append: String = "\u2026"): String =
    if (txt.length <= len) txt else {
      //truncateRegex.replaceAllIn(txt.take(len), "")+append
      val lastSpace = txt.lastIndexOf(" ", len)
      txt.substring(0, if (lastSpace < 0) len else lastSpace)+append
    }

  def updateLinks(content: String, f: String => String) =
    ahrefRegex.replaceAllIn(content, m => Regex.quoteReplacement(f(m.group(1))))

  //private val classRegex = """(?x) class=(?: ("|')([\w\ ]+?)\1 | (\w+) )""".r
  //private val idRegex    = """(?x) id=   (?: ("|')([\w\ ]+?)\1 | (\w+) )""".r
  //private val tagRegex   = """\<([a-zA-Z]\w*?)\W""".r
  def classesAndTags(html: String): Set[String] = {
    //val classes = classRegex.findAllMatchIn(html).map { m => if (m.group(2) != null) m.group(2) else m.group(3) }.flatMap(_.split("\\s+"))
    //val ids     = idRegex   .findAllMatchIn(html).map { m => if (m.group(2) != null) m.group(2) else m.group(3) }.flatMap(_.split("\\s+"))
    //val tags: Iterator[String]    = tagRegex.findAllMatchIn(html).map(_.group(1))
    //(tags ++ classes.map("."+_) ++ ids.map("#"+_)).toSet

    // following code is just a faster version of regexes above
    val idents = collection.mutable.Set[String]()

    var pos = html.indexOf('<', 0)
    while (pos != -1) {
      var i = pos+1
      while (i < html.length && Character.isLetterOrDigit(html.charAt(i))) { i += 1 }
      if (i > pos+1) { idents += html.substring(pos+1, i) }
      pos = html.indexOf('<', i)
    }

    def p(prelude: String, prefix: String) = {
      var pos = html.indexOf(prelude, 0)
      while (pos != -1) {
        pos = pos+prelude.length
        var i = pos

        val q = html.charAt(i)
        if (q == '"' || q == '\'') {
          i = html.indexOf(q, i+1)
          for (c <- html.substring(pos+1, i).split(" ")) {
            idents += prefix+c
          }

        } else {
          while (i < html.length && Character.isLetterOrDigit(html.charAt(i))) { i += 1 }
          idents += prefix+html.substring(pos, i)
        }

        pos = html.indexOf(prelude, i)
      }
    }

    p("class=", ".")
    p("id=",    "#")
    idents.toSet
  }
}


case class FlowLayout(baseUrl: String, base: Base, blog: Blog, markup: Markup, mill: FlowLayoutMill) extends Layout {
  import FlowLayout._

  def rel(url: String): String = blog.relativize(url, baseUrl)
  def txl(s: String) = blog.translation(s)
  def ifs(c: Boolean, body: => String) = if (c) body else ""
  def ifs(x: String, body: => String) = if (x != null && x.nonEmpty) body else ""
  def ifs(x: Any, body: => String) = if (x != null) body else ""
  def ifs(x: String) = if (x != null) x else ""

  private def plaintextDescription(a: Article): String =
    stripTags(a.text.firstParagraph).replace('\n', ' ')

  private def mainImageUrl(a: Article): String  = a.images.find(_.mods == "main").map(_.url).getOrElse(null)
  private def otherImageUrl(a: Article): String = a.images.headOption.map(_.url).getOrElse(null)

  def imgTag(img: Image, t: Text, showDesc: Boolean = true, linkTo: String = null) = {
    val (cl, srcPath) = img match {
      case i if i.mods == "main" && i.align == ">" => ("fr", blog.bigThumbnailUrl(img, true))
      case i if i.mods == "main" => ("main", blog.bigThumbnailUrl(img, false))
      case i => ("thz", blog.thumbnailUrl(img))
    }
    val desc = if (showDesc) {
      val title   = ifs(img.title, t.paragraph(img.title)).trim
      val tags    = makeTagLinks(img.tags.visible.map(base.tagByTitle)).trim
      val source  = ifs(img.source, s"""(<a href="${img.source}">${txl("source")}</a>)""")
      val license = (ifs(img.license)+" "+source).trim
      val locSrc  = ifs(img.localSource, articleLink(img.localSource, img.localSource.title))
      Seq(title, tags, license, locSrc).mkString(" ").replaceAll(" +", " ").trim
    } else ""

    val src  = rel(blog.absUrlFromPath(srcPath))
    val href = rel(if (linkTo == null) img.url else linkTo)

    val imgTag = s"""<img class=thz ${ifs(img.alt, s"title='${img.alt}' ") }src="$src"/>"""
    val aTag   = if (!img.zoomable && linkTo == null) imgTag else s"""<a href="$href">$imgTag</a>"""

    s"""<span class=$cl>$aTag$desc</span>"""
  }

  def gallerySample(a: Article) =
    a.extraImages.take(3).map { i =>
      s"""<a href="${blog.relUrlFromSlug(a.slug)}"><img class=th src="${blog.thumbnailUrl(i)}"/></a>"""
    }.mkString(" ")

  def rssLink(rss: String) =
    s"""<link rel="alternate" type="application/rss+xml" href="${rel(rss)}"/>"""

  def ogTags(a: Article): String = {
    if (blog.hasOgTags) {
      val mainImg  = mainImageUrl(a)
      val otherImg = otherImageUrl(a)
      val (tpe, img) =
        if (mainImg != null) ("summary_large_image", mainImg)
        else                 ("summary", otherImg)

      """<meta name="twitter:card" content=""""+tpe+""""/>""" +
      """<meta property="og:type" content="article"/>""" +
      """<meta property="og:url" content=""""+escape(baseUrl)+""""/>""" +
      """<meta property="og:title" content=""""+escape(a.title)+""""/>""" +
      """<meta property="og:description" content=""""+escape(truncate(plaintextDescription(a), 200))+""""/>""" +
      ifs(img,                 """<meta property="og:image" content=""""+escape(img)+""""/>""") +
      ifs(blog.twitterSite,    """<meta name="twitter:site" content=""""+escape(blog.twitterSite)+""""/>""") +
      ifs(blog.twitterCreator, """<meta name="twitter:creator" content=""""+escape(blog.twitterCreator)+""""/>""")
    } else ""}


  def makePage(content: String, title: String = null, containImages: Boolean = false, headers: String = null, includeCompleteStyle: Boolean = false, article: Option[Article] = None): String = {
    val h = blog.hooks.header(base, blog, this, article)
    val header = if (h != null) h else mill.header.get(rel)
    val footer = mill.footer.get(rel)
    val body = "<body><div class=b>"+header+content+footer+"</div></body>"
    val cats: Set[String] = if (includeCompleteStyle) null else classesAndTags(body)

s"""<!DOCTYPE html>
<html${ifs(blog.hasOgTags, " prefix=\"og: http://ogp.me/ns#\"")}>
<head>
<meta charset="utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>${ifs(title, title+" | ")+blog.title}</title>
${rssLink("rss.xml")}
${ifs(headers)}
${if (blog.cssFile == null) { s"""<style>${mill.style(cats)}</style>""" }
  else { s"""<link rel="stylesheet" href="${rel("style.css")}" type="text/css"/>""" } }
${ifs(containImages, s"<script>$galleryScript</script>")}
</head>$body
</html>"""
  }

  def makeIndex(fullArticles: Seq[Article], links: Seq[Article], archiveLinks: Seq[Article] = Seq(), groupArchiveByMonth: Boolean = false, tagsToShow: Seq[Article] = Seq()): String =
    blog.hooks.indexPrepend(base, blog, this, fullArticles, archiveLinks.nonEmpty)+
    fullArticles.take(1).map(_makeFullArticle(_, true)).mkString("\n")+
    blog.hooks.afterFirstArticle(base, blog, this, fullArticles, archiveLinks.nonEmpty)+
    (if (tagsToShow.nonEmpty) "<aside>"+tagsToShow.map(makeTagLink).mkString(" ")+"</aside>" else "")+
    fullArticles.drop(1).map(_makeFullArticle(_, true)).mkString("\n")+
    listOfLinks(links, blog.archiveFormat == "short")+"<br/>"+
    (if (!groupArchiveByMonth) "<div style='clear:both'>"+listOfLinks(archiveLinks, false)+"</div>" else groupArchive(archiveLinks))+"<br/>"


  private def groupArchive(archiveLinks: Seq[Article]) = {
    val (undated, dated) = archiveLinks.partition(_.date == null)
    "<div style='clear:both'>"+
    dated.groupBy(a => year(a.date)).toSeq.sortBy(_._1).reverse.map { case (y, as) =>
      val mas = if (y < year(new Date)) {
        (1 to 12).map { m => (m, as.find(a => month(a.date) == m)) }
      } else {
        as.reverse.map { a => (month(a.date), Some(a)) }
      }
      y+" "+mas.map {
        case (m, Some(a)) => articleLink(a, "&nbsp;"+m+"&nbsp;")
        case (m, None) => "&nbsp;"+m+"&nbsp;"
      }.mkString(" ")
    }.mkString("<br/>")+
    undated.map { a => "<br/>"+articleLink(a, txl("undated")) }.mkString+
    "</div>"
  }

  def makeFullArticle(a: Article): String = _makeFullArticle(a, false)

  // Bare article body, no title, no links to related articles, no tags. This method is used in RSS feeds.
  def makeArticleBody(a: Article, compact: Boolean): String = {
    a.text.render(this, rel)+
    ifs(a.isTag, {
      val linked = a.slugsOfLinkedArticles(blog).toSet
      listOfLinks(base.allTags(a.asTag)._2.filter(a => !linked.contains(a.asSlug)), blog.tagFormat == "short")
    })+
    ifs(!compact, a.extraImages.map(img => imgTag(img.asSmallThumbnail, if (img.localSource != null) img.localSource.text else a.text)).mkString(" "))+
    ifs( compact, gallerySample(a))
  }

  def listOfLinks(list: Seq[Article], shortArticles: Boolean) =
    if (shortArticles) list.map(makeShortArticle).mkString+"<br/>&nbsp;"
    else {
      val l = blog.hooks.list(base, blog, this, list)
      if (l != null) l else list.map(makeLink).mkString("<br/>")+"<br/>"
    }

  def rowOfLinks(list: Seq[Article]) =
    list.map(a => articleLink(a, a.title)).mkString(", ")

  def makeShortArticle(a: Article): String = "<div class=sh>"+makeTitle(a)+"<br/>"+makeShortArticleBody(a)+"</div>"

  def makeShortArticleBody(a: Article): String = {
    val img = a.images.find(_.mods == "main").map(i => imgTag(i.asSmallThumbnail, a.text, false, blog.absUrl(a))).getOrElse("")
    val txt = truncate(plaintextDescription(a), 300)

    ifs(img, s"<div class=shimg>$img</div> ")+txt+" "+articleLink(a, txl("continueReading"))
  }

  private def _makeFullArticle(a: Article, compact: Boolean): String = {
    val title = blog.hooks.title(base, blog, this, a, compact)

    "<article>"+
    ifs(!compact, "<span class=f>"+makeNextPrevArrows(a)+"</span>")+
    (if (title != null) title else if (!a.isTag) makeTitle(a, compact) else txl("tagged")+" "+makeTitle(a)+"<br/>")+
    ifs(a.isTag, {
      val sup = a.tags.visible.map(base.tagByTitle)
      val sub = base.allTags(a.asTag)._2.filter(_.isTag)
      ifs(sub.nonEmpty || sup.nonEmpty, {
        "<div style='font-size:0.85em'>"+
        ifs(sup.nonEmpty, txl("supersections")+" "+rowOfLinks(sup)+" ")+
        ifs(sub.nonEmpty, txl("subsections")  +" "+rowOfLinks(sub))+
        "</div>"
      })
    })+
    ifs(title == null, "<br/>\n")+
    makeArticleBody(a, compact)+
    ifs(!compact, blog.hooks.fullArticleBottom(base, blog, this, a))+
    "</article>"+
    ifs(!compact,
      "<footer>"+
      ifs(blog.allowComments && !a.isTag, s"""<b><a href="${ rel(blog.absUrlFromPath("comments.php?url="+blog.relUrlFromSlug(a.slug))) }">${txl("comments.enter")}</a></b>""")+
      ifs(blog.shareLinks && !a.isTag, {
        val url = URLEncoder.encode(blog.absUrl(a), "UTF-8")
        s""" &nbsp;&nbsp; ${txl("share.share")} <a href="https://www.facebook.com/sharer/sharer.php?u=$url">${txl("share.facebook")}</a>, <a href="https://twitter.com/intent/tweet?url=$url">${txl("share.twitter")}</a>"""
      })+
      ifs((blog.allowComments || blog.shareLinks) && !a.isTag, "<hr/>")+
      ifs(a.tags.visible.nonEmpty, "<p>"+txl("tags")+" "+makeTagLinks(a.tags.visible.sortBy(!_.supertag).map(base.tagByTitle), a)+"</p>")+
      //ifs(a.license, a.license+"<br/>")+
      ifs(a.pubArticles.nonEmpty,"<p>"+txl("published")  +"<br/>"+a.pubArticles.map(makeLink).mkString("<br/>")+"</p>")+
      ifs(a.pubBy != null,       "<p>"+txl("publishedBy")+" "    +articleLink(a.pubBy, makeDate(a), allowImageMarker = true)+"</p>")+
      ifs(a.similar.nonEmpty,    "<p>"+(if (a.isTag) txl("similarTags") else txl("similar"))+
                                                                  " "+a.similar.map(s => articleLink(s, s.title, allowImageMarker = true)).mkString(", ")  +"</p>")+
      ifs(a.backlinks.nonEmpty,  "<p>"+txl("backlinks")  +" "+a.backlinks.map(s => articleLink(s, s.title, allowImageMarker = true)).mkString(", ")+"</p>")+
      "</footer>"
    )
  }

  def makeTagIndex(base: Base) =
    base.allTags.toSeq
      .filter { case (_, (t, as)) => t.images.size > 0 || as.size > 0 }
      .sortBy { case (_, (t, as)) => (~as.size, t.slug) }
      .map { case (_, (t, as)) => makeTagLink(t)+" ("+as.size+")" }.mkString(" ")

  def addArrows(content: String, next: Article, prev: Article, includeBottom: Boolean = false) = {
    "<span class=f>"+_makeNextPrevArrows(next, prev)+"</span>"+
    content+
    (if (includeBottom) "<span class=f>"+_makeNextPrevArrows(next, prev)+"</span>" else "")
  }

  private def makeDateWithLinkToPubBy(a: Article, asLink: Boolean) =
    if (a.pubBy != null && !asLink) articleLink(a.pubBy, makeDate(a))
    else makeDate(a)

  def makeTitle(a: Article, asLink: Boolean = true) =
    makeDateWithLinkToPubBy(a, asLink)+" <h2>"+articleLink(a, a.title, asLink)+"</h2>"

  def articleUrl(a: Article) = if (a.link != null) a.link else rel(blog.absUrl(a))

  def articleLink(a: Article, title: String, asLink: Boolean = true, allowImageMarker: Boolean = false) =
    if (a.link != null || asLink)
      s"""<i><a href="${articleUrl(a)}">$title</a></i>"""+ifs(allowImageMarker && a.hasImageMarker, blog.imageMarker)
    else
      s"""<i>$title</i>"""+ifs(allowImageMarker && a.hasImageMarker, blog.imageMarker)

  def makeLink(a: Article) = {
    val title = blog.hooks.listTitle(base, blog, this, a)
    if (title != null) title
    else makeDate(a)+" "+articleLink(a, a.title, allowImageMarker = true)
  }

  def makeTagLink(t: Article) = {
    val html = s"""#<i><a href="${rel(blog.absUrl(t))}">${t.title}</a></i>"""
    if (t.isSupertag) s"<b>$html</b>" else html
  }

  def makeNextPrevArrows(a: Article) = {
    val (prev, next) = (base.prev(a), base.next(a))
    if (prev == null && next == null) "" else  _makeNextPrevArrows(prev, next)
  }

  def _makeNextPrevArrows(prev: Article, next: Article) =
    (if (prev == null) "«««" else s"""<a href="${rel(blog.absUrl(prev))}">«««</a>""")+" "+
    (if (next == null) "»»»" else s"""<a href="${rel(blog.absUrl(next))}">»»»</a>""")

  def makeTagLinks(tags: Seq[Article], a: Article = null) =
    tags.map { t =>
      makeTagLink(t) + ifs(t.isSupertag && a != null,
        ifs(base.prev(a, t.asTag), s""" <a href="${rel(blog.absUrl(base.prev(a, t.asTag)))}">««</a>""")+
        ifs(base.next(a, t.asTag), s""" <a href="${rel(blog.absUrl(base.next(a, t.asTag)))}">»»</a>""")
      )
    }.mkString(" ")

  def makeDate(a: Article) = a.date match {
    case null => ""
    case d if year(a.date) == year(new Date) =>
      new SimpleDateFormat("d. M.").format(d)
    case d =>
      new SimpleDateFormat("d. M. yyyy").format(d)
  }
}


class PrepatedText(html: String, resolver: String => String) {
  private val arr = {
    val res = new collection.mutable.ArrayBuffer[String]
    val m = ahrefRegex.pattern.matcher(html)
    var pos = 0
    while (m.find()) {
      res += html.substring(pos, m.start)
      res += resolver(m.group)
      pos = m.end
    }
    res += html.substring(pos)
    res.result
  }

  def get(rel: String => String) = {
    val sb = new StringBuilder()
    var i = 0; while (i < arr.length-1) {
      sb.append(arr(i))
      sb.append(rel(arr(i+1)))
      i += 2
    }
    sb.append(arr.last)
    sb.toString
  }
}
