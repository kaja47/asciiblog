package asciiblog

import MakeFiles.galleryScript
import AsciiPatterns.ahrefRegex
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.net.URLEncoder
import scala.util.matching.Regex


trait LayoutMill {
  def make(baseUrl: String): Layout
  def basicStyle: String
}

trait Layout {
  def baseUrl: String
  def makePage(content: String, title: String = null, containImages: Boolean = false, ogTags: Article = null, rssLink: String = null, includeCompleteStyle: Boolean = false, article: Option[Article] = None): String

  def makeIndex(parts: Seq[PagePart]): String
  def makeArchive(a: Article, parts: Seq[PagePart]): String
  def makeFullArticle(a: Article, parts: Seq[PagePart] = Seq()): String
  def makeTagIndex(tags: Seq[(Article, Int)]): String

  def articleUrl(a: Article): String
  def articleLink(a: Article, title: String, asLink: Boolean = true, imgMarker: Boolean = false): String
}



sealed trait PagePart
object PagePart {
  case class FullArticles(articles: Seq[Article]) extends PagePart
  case class Summaries(articles: Seq[Article]) extends PagePart
  case class Links(articles: Seq[Article]) extends PagePart
  case class Bodies(articles: Seq[Article]) extends PagePart
  case class Tags(tags: Seq[Article]) extends PagePart
  case class Archive(archive: Seq[Article], groupedBy: String) extends PagePart
  case class Text(txt: String) extends PagePart
}



class FlowLayoutMill(base: Base, blog: Blog, val resolver: String => String) extends LayoutMill {
  def make(baseUrl: String): FlowLayout = FlowLayout(baseUrl, base, blog, this)

  def basicStyle = s"""
a          { color: inherit; }
.r         { text-align: right; }
.f         { float: right; }
.b         { max-width: 46em; font-family: sans-serif; line-height: 1.3; margin: 0 auto; }
blockquote { margin:0; padding:0; font-style:italic; }
.about     { text-decoration: dotted underline; }
.thz, .thr, .fr, .main
           { font-size: .8em }
span.thz   { width: ${blog.thumbWidth}px; display: inline-block; vertical-align: top; }
span.thr   { width: ${blog.thumbWidth}px; display: inline-block; vertical-align: top; float: right; }
span.fr    { text-align: right; max-width: 45%; float: right; padding-left: 0.5em; }
span.main  { text-align: center; display: block; margin-bottom: .5em; }
span.main img, span.fr img
           { max-width: 100%; }
h2         { display: inline; margin: 0; font-size:1em; }
hr         { border: 0; border-top: 1px dotted gray; margin: .8em 4em; }
p          { margin: 1.4em 0; }
.sh        { float: left; clear: both; margin: .8em 0; }
.shimg     { float: left; margin: 0 .5em 0 0; }
footer     { font-size: 0.9em; border-top: 1px dashed gray; padding-top: 1em; color: #222; clear: both; }
article    { margin: 2em 0 4em 0; }
aside      { margin-bottom: 2em; clear:both; font-size: .9em; }
pre        { white-space: pre-wrap; overflow-wrap: break-word; }
""".trim

  val css = CSSMinimizeJob(basicStyle + "\n" + blog.cssStyle)

  val defaultHeader = s"""<div class=r><b><a href="index">${blog.title}</a></b> [<a href="rss.xml">RSS</a>]</div>"""
  val header = new PrepatedText(if (blog.header.nonEmpty) blog.header else defaultHeader, resolver)
  val footer = new PrepatedText(blog.footer, resolver)
}


object FlowLayout {
  def truncate(txt: String, len: Int, append: String = "\u2026"): String =
    if (txt.length <= len) txt else {
      val lastSpace = txt.lastIndexOf(" ", len)
      txt.substring(0, if (lastSpace < 0) len else lastSpace)+append
    }

  def updateLinks(content: String, f: String => String) =
    ahrefRegex.replaceAllIn(content, m => Regex.quoteReplacement(f(m.group(1))))
}


case class FlowLayout(baseUrl: String, base: Base, blog: Blog, mill: FlowLayoutMill) extends Layout {
  import FlowLayout._

  private final val nbsp = "\u00A0"

  def rel(url: String): String = blog.relativize(url, baseUrl)
  def txl(s: String) = blog.translation(s)
  def ifs(c: Boolean, body: => String) = if (c) body else ""
  def ifs(x: String, body: => String) = if (x != null && x.nonEmpty) body else ""
  def ifs(x: Any, body: => String) = if (x != null) body else ""
  def ifs(x: String) = if (x != null) x else ""



  def makePage(content: String, title: String = null, containImages: Boolean = false, ogTags: Article = null, rssLink: String = null, includeCompleteStyle: Boolean = false, article: Option[Article] = None): String = {
    val h = blog.hooks.header(base, blog, this, article)
    val header = if (h != null) h else mill.header.get(rel)
    val footer = mill.footer.get(rel)
    val body = "<div class=b>"+header+content+footer+"</div>"

    "<!DOCTYPE html>\n"+
    "<html"+ ifs(blog.hasOgTags, " prefix=\"og: http://ogp.me/ns#\"") + ">\n"+
    "<meta charset=utf-8>"+
    "<meta name=viewport content=\"width=device-width,initial-scale=1\">"+
    "<title>"+ ifs(title, title+" | ")+blog.title+"</title>"+
    makeRssLink("rss.xml")+
    (if (blog.cssFile != null || blog.cssExport)
      "<link rel=stylesheet href="+html.quoteAttribute(rel("style.css"))+" type=text/css>"
    else
      "<style>"+mill.css.styleFor(body, !includeCompleteStyle)+"</style>"
    )+
    ifs(containImages,
      if (blog.galleryScriptExport) {
        "<script src="+html.quoteAttribute(rel("gallery.js"))+"></script>"
      } else {
        "<script>"+galleryScript+"</script>"
      }
    )+"\n"+
    ifs(rssLink, makeRssLink(rssLink)+"\n")+
    ifs(blog.hasOgTags && ogTags != null, makeOgTags(ogTags)+"\n")+
    body
  }

  def makeIndex(parts: Seq[PagePart]): String =
    renderParts(parts)




  def makeArchive(a: Article, parts: Seq[PagePart]): String =
    makeNextPrevArrows(a.next, a.prev)+
    "<article>"+
    "<h2>"+html.escape(a.title)+"</h2><br><br>"+
    renderParts(parts)+
    "</article>"+
    makeNextPrevArrows(a.next, a.prev)


  private def renderParts(parts: Seq[PagePart]): String =
    parts.map(renderPart).mkString("\n")

  private def renderPart(part: PagePart): String =
    part match {
      case PagePart.FullArticles(articles) =>
        articles.map(a => _makeFullArticle(a, true, Seq())).mkString("\n")
      case PagePart.Summaries(articles) =>
        listOfSummaries(articles)
      case PagePart.Links(articles) =>
        listOfLinks(articles)
      case PagePart.Bodies(articles) =>
        articles.map(_.text.render(rel)).mkString
      case PagePart.Tags(tags) =>
        "<aside>"+tags.map(makeTagLink).mkString(" ")+"</aside>"
      case PagePart.Archive(archive, groupedBy) =>
        groupedBy match {
          case "year"  => "<div class=archive style=clear:both>"+archive.map(a => articleLink(a, a.title)).mkString("<br>")+"</div>"
          case "month" => "<div class=archive style=clear:both>"+groupArchive(archive)+"</div>"
        }
      case PagePart.Text(txt) => txt
    }

  private def groupArchive(archiveLinks: Seq[Article]) = {
    val (undated, dated) = archiveLinks.partition(_.date == null)
    dated.groupBy(a => a.date.getYear).toSeq.sortBy(_._1).reverse.map { case (y, as) =>
      val mas = if (y < LocalDateTime.now().getYear) {
        (1 to 12).map { m => (m, as.find(a => a.date.getMonthValue == m)) }
      } else {
        as.reverse.map { a => (a.date.getMonthValue, Some(a)) }
      }
      y+" "+mas.map {
        case (m, Some(a)) => articleLink(a, nbsp+m+nbsp)
        case (m, None) => nbsp+m+nbsp
      }.mkString(" ")
    }.mkString("<br>")+
    undated.map { a => "<br>"+articleLink(a, txl("undated")) }.mkString
  }

  def makeFullArticle(a: Article, parts: Seq[PagePart] = Seq()): String = _makeFullArticle(a, false, parts)


  def listOfLinks(list: Seq[Article]) = {
    val l = blog.hooks.list(base, blog, this, list)
    if (l != null) l else list.map(makeLink).mkString("<br>")+"<br>"
  }

  def listOfSummaries(list: Seq[Article]) =
    list.map(makeSummary).mkString("\n")+"\n"

  def makeSummary(a: Article): String = "<div class=sh>"+makeTitle(a)+"<br>"+makeSummaryBody(a)+"</div>"

  def makeSummaryBody(a: Article): String = {
    val img = a.images.find(_.mods == "main").map(i => imgLinkTag(i.asSmallThumbnail, blog.absUrl(a))).getOrElse("")
    val txt = truncate(a.text.plaintextSummary, 300)

    ifs(img, s"<div class=shimg>$img</div> ")+txt+" "+articleLink(a, txl("continueReading"))
  }

  private def imgLinkTag(img: Image, linkTo: String) = {
    val src = rel(blog.absUrlFromPath(blog.thumbnailUrl(img)))
    val imgTag = "<img class=thz src="+html.quoteAttribute(src)+">"
    val a      = "<a href="+html.quoteAttribute(rel(linkTo))+">"+imgTag+"</a>"
    s"""<span class=thz>$a</span>"""
  }

  private def _makeFullArticle(a: Article, compact: Boolean, parts: Seq[PagePart]): String = {
    val title = blog.hooks.title(base, blog, this, a, compact)

    "<article>"+
    ifs(!compact && (a.prev != null || a.next != null), makeNextPrevArrows(a.prev, a.next))+
    (if (title != null) title else if (!a.isTag) makeTitle(a, compact) else txl("tagged")+" "+makeTitle(a)+"<br>")+
    ifs(a.isTag, {
      val sup = a.tags.visible.map(base.tagByTitle)
      val sub = base.allTags(a.asTag)._2.filter(_.isTag)
      ifs(sub.nonEmpty || sup.nonEmpty, {
        "<div style=font-size:0.85em>"+
        ifs(sup.nonEmpty, txl("supersections")+" "+sup.map(articleLink(_)).mkString(", ")+" ")+
        ifs(sub.nonEmpty, txl("subsections")  +" "+sub.map(articleLink(_)).mkString(", "))+
        "</div>"
      })
    })+
    ifs(title == null, "<br>\n")+
    a.text.render(rel)+
    renderParts(parts)+
    ifs(a.isTag, a.taggedImages.map { img => imgLinkTag(img, blog.absUrl(img.localSource)) }.mkString(""))+
    ifs(!compact, blog.hooks.fullArticleBottom(base, blog, this, a))+
    "</article>"+
    ifs(!compact,
      "\n<footer>"+
      ifs(blog.allowComments && !a.isTag, s"""<b><a href="${rel(blog.absUrlFromPath("comments.php?u="+a.slug)) }">${txl("comments.enter")}</a></b>""")+
      ifs(blog.shareLinks && !a.isTag, {
        if (!blog.allowShareScript) {
          val url = URLEncoder.encode(blog.absUrl(a), "UTF-8")
          s""" $nbsp ${txl("share.share")}"""+
          s""" <a href="https://www.facebook.com/sharer/sharer.php?u=$url">${txl("share.facebook")}</a>,"""+
          s""" <a href="https://twitter.com/intent/tweet?url=$url">${txl("share.twitter")}</a>,"""+
          s""" <a href="https://www.linkedin.com/shareArticle?mini=true&url=$url">${txl("share.linkedin")}</a>,"""+
          s""" <a href="https://www.tumblr.com/share/link?url=$url">${txl("share.tumblr")}</a>"""
      } else {
          val slug = URLEncoder.encode(a.slug, "UTF-8")
          s""" $nbsp ${txl("share.share")} """+
          "<a href="+html.quoteAttribute(rel("share.php")+"?f="+slug)+">"+txl("share.facebook")+"</a>, "+
          "<a href="+html.quoteAttribute(rel("share.php")+"?t="+slug)+">"+txl("share.twitter") +"</a>, "+
          "<a href="+html.quoteAttribute(rel("share.php")+"?l="+slug)+">"+txl("share.linkedin")+"</a>, "+
          "<a href="+html.quoteAttribute(rel("share.php")+"?u="+slug)+">"+txl("share.tumblr")+"</a>"
        }
      })+
      ifs((blog.allowComments || blog.shareLinks) && !a.isTag, "<hr>")+
      ifs(a.tags.visible.nonEmpty, "<p>"+txl("tags")       +" "   +makeTagLinks(a.tags.visible.sortBy(!_.supertag).map(base.tagByTitle)))+
      ifs(a.pubArticles.nonEmpty,  "<p>"+txl("published")  +"<br>"+a.pubArticles.map(makeLink).mkString("<br>")                         )+
      ifs(a.pubBy != null,         "<p>"+txl("publishedBy")+" "   +articleLink(a.pubBy, makeDate(a), imgMarker = true)                  )+
      ifs(a.similar.nonEmpty,
        "<p>"+
        (if (a.isTag) txl("similarTags") else txl("similar"))+" "
        +a.similar.map { s => articleLink(s.article, s.article.title, imgMarker = true)/*+" ("+s.commonTags+", "+s.dateDiff+"d)"*/ }.mkString(", ")
      )+
      ifs(a.backlinks.nonEmpty,    "<p>"+txl("backlinks")  +" "   +a.backlinks.map(s => articleLink(s, s.title, imgMarker = true)).mkString(", "))+
      ifs(a.foreighBacklinks.nonEmpty,"<p>"+txl("foreignBacklinks")+" "  +a.foreighBacklinks.map(b => aTag(b.title, b.url)+" ("+b.site+")").mkString(", "))+
      //ifs(a.license, "<p>"+a.license+"<br>")+
      "</footer>"
    )
  }

  def makeTagIndex(tags: Seq[(Article, Int)]) =
    tags.map { case (t, count) => makeTagLink(t)+" ("+count+")" }.mkString(" ")

  private def makeDateWithLinkToPubBy(a: Article, asLink: Boolean) =
    if (a.pubBy != null && !asLink) articleLink(a.pubBy, makeDate(a)) else makeDate(a)

  def makeTitle(a: Article, asLink: Boolean = true) =
    makeDateWithLinkToPubBy(a, asLink)+" <h2>"+articleLink(a, a.title, asLink)+"</h2>"

  def articleUrl(a: Article) = rel(articleAbsUrl(a))
  def articleAbsUrl(a: Article) = if (a.link != null) a.link else blog.absUrl(a)

  def articleLink(a: Article, title: String = null, asLink: Boolean = true, imgMarker: Boolean = false) = {
    val _title = html.escape(if (title == null) a.title else title)
    (if (a.link != null || asLink) aTag(_title, articleAbsUrl(a)) else _title)+ifs(imgMarker && a.hasImageMarker, " "+blog.imageMarker)
  }

  def makeLink(a: Article) = {
    val title = blog.hooks.listTitle(base, blog, this, a)
    if (title != null) title else makeDate(a)+" "+articleLink(a, a.title, imgMarker = true)
  }

  def makeTagLink(t: Article) =
    if (!t.isSupertag) "#"+aTag(html.escape(t.title), blog.absUrl(t))
    else            "<b>#"+aTag(html.escape(t.title), blog.absUrl(t))+"</b>"

  def makeTagLinks(tags: Seq[Article]) = tags.map(makeTagLink).mkString(" ")

  def makeNextPrevArrows(prev: Article, next: Article) =
    "<span class=f>"+
    (if (prev == null) "«««" else aTag("«««", blog.absUrl(prev)))+" "+
    (if (next == null) "»»»" else aTag("»»»", blog.absUrl(next)))+
    "</span>"

  def aTag(title: String, href: String) =
    "<a href="+html.quoteAttribute(rel(href))+">"+title+"</a>"

  private val shortDate = DateTimeFormatter.ofPattern("d. M.")
  private val longDate  = DateTimeFormatter.ofPattern("d. M. yyyy")
  private val thisYear  = LocalDateTime.now().getYear

  def makeDate(a: Article) = a.date match {
    case null => ""
    case d if a.date.getYear == thisYear => shortDate.format(d)
    case d => longDate.format(d)
  }


  private def mainImageUrl(a: Article): String  = a.images.find(_.mods == "main").map(_.url).getOrElse(null)
  private def otherImageUrl(a: Article): String = a.images.headOption.map(_.url).getOrElse(null)

  def makeRssLink(rss: String) = {
    val w = new XMLSW(new java.lang.StringBuilder(100), true)
    (w.shortElement2("link") { _.attr("rel", "alternate").attr("type", "application/rss+xml").attr("href", rel(rss)) }).builder.toString
  }

  def makeOgTags(a: Article): String = {
    val mainImg  = mainImageUrl(a)
    val otherImg = otherImageUrl(a)
    val (tpe, img) =
      if (mainImg != null)       ("summary_large_image", mainImg)
      else if (otherImg != null) ("summary",             otherImg)
      else if (blog.textCards)   ("summary_large_image", blog.absUrlFromPath(blog.textCardUrl(a)))
      else                       ("summary",             null)

    val sb = new java.lang.StringBuilder(300)
    val w = new XMLSW(sb, html5 = true)
    w.shortElement2("meta") { _.attr("name",     "twitter:card"  ).attr("content", tpe) }
    w.shortElement2("meta") { _.attr("property", "og:type"       ).attr("content", "article") }
    w.shortElement2("meta") { _.attr("property", "og:url"        ).attr("content", baseUrl) }
    w.shortElement2("meta") { _.attr("property", "og:title"      ).attr("content", a.title) }
    w.shortElement2("meta") { _.attr("property", "og:description").attr("content", truncate(a.text.plaintextSummary, 200)) }

    if(img != null) {
      w.shortElement2("meta") { _.attr("property", "og:image").attr("content", img) }
    }
    if(blog.twitterSite.nonEmpty) {
      w.shortElement2("meta") { _.attr("name", "twitter:site").attr("content", blog.twitterSite) }
    }
    if(blog.twitterCreator.nonEmpty) {
      w.shortElement2("meta") { _.attr("name", "twitter:creator").attr("content", blog.twitterCreator) }
    }
    sb.toString
  }
}



class PrepatedText(htmlText: String, resolver: String => String) {
  val ahrefRegex  = """(?x) (?<= href=) ("|') (.*?) \1 """.r

  private val arr = {
    val res = new collection.mutable.ArrayBuffer[String]
    val m = ahrefRegex.pattern.matcher(htmlText)
    var pos = 0
    while (m.find()) {
      res += htmlText.substring(pos, m.start)
      res += resolver(m.group(2))
      pos = m.end
    }
    res += htmlText.substring(pos)
    res.result
  }

  def get(rel: String => String) = {
    val sb = new StringBuilder()
    var i = 0; while (i < arr.length-1) {
      sb.append(arr(i))
      sb.append(html.quoteAttribute(rel(arr(i+1))))
      i += 2
    }
    sb.append(arr.last)
    sb.toString
  }
}
