package asciiblog

import MakeFiles. { year, month, galleryScript, isAbsolute }
import AsciiText. { ahrefRegex, ahrefCheck, imgsrcRegex, imgsrcCheck }
import java.text.SimpleDateFormat
import java.util.Date
import java.net.URLEncoder
import scala.util.matching.Regex
import util.escape


trait LayoutMill {
  def make(baseUrl: String): Layout
}

trait Layout extends ImageLayout {
  def makePage(content: String, title: String = null, containImages: Boolean = false, headers: String = null, includeCompleteStyle: Boolean = false): String
  def makeIndex(fullArticles: Seq[Article], links: Seq[Article], archiveLinks: Seq[Article] = Seq(), groupArchiveByMonth: Boolean = false): String
  def makeFullArticle(a: Article): String
  def makeTagIndex(base: Base): String
  def addArrows(content: String, next: Article, prev: Article, includeBottom: Boolean = false): String
}

// this type is passed into markup
trait ImageLayout {
  def imgTag(img: Image, t: Text, showDesc: Boolean = true, linkTo: String = null): String
}



class FlowLayoutMill(base: Base, blog: Blog, markup: Markup) {
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
.low       { font-size: 0.9em; }
""".trim

  import CssMinimizer._
  private val optStyle = optimize(parseCSS(inlineStyles) ++ parseCSS(blog.cssStyle))

  def style(cats: Set[String]) = cats match {
    case null => render(optStyle)
    case cats => render(minimize(optStyle, cats))
  }
}


object FlowLayout {
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
}


case class FlowLayout(baseUrl: String, base: Base, blog: Blog, markup: Markup, layoutMill: FlowLayoutMill) extends Layout {
  import FlowLayout._

  def rel(url: String): String =
    if (baseUrl == null || url.startsWith("#")) url
    else blog.relativize(url, baseUrl)

  def txl(s: String) = blog.translation(s)
  def ifs(c: Boolean, body: => String) = if (c) body else ""
  def ifs(x: String, body: => String) = if (x != null && x.nonEmpty) body else ""
  def ifs(x: Any, body: => String) = if (x != null) body else ""
  def ifs(x: String) = if (x != null) x else ""

  def eval(engine: javax.script.ScriptEngine, args: Map[String, Any]) = {
    val res = engine.asInstanceOf[javax.script.Invocable].invokeFunction("run", Map(
      "base" -> base, "blog" -> blog, "layout" -> this,
    ) ++ args)
    ifs(res, res.toString)
  }

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

    val imgTag = s"""<img class=thz ${ifs(img.alt, s"title='${img.alt}' ") }src="${blog.absUrlFromPath(srcPath)}"/>"""
    val aTag   = s"""<a href="${if (linkTo == null) img.url else linkTo}">$imgTag</a>"""
    s"""<span class=$cl>$aTag$desc</span>"""
  }

  def gallerySample(a: Article) =
    a.extraImages.take(3).map { i =>
      s"""<a href="${blog.relUrlFromSlug(a.slug)}"><img class=th src="${blog.thumbnailUrl(i)}"/></a>"""
    }.mkString(" ")

  val classRegex = """(?x) class=(?: ("|')([\w\ ]+?)\1 | (\w+) )""".r
  val idRegex    = """(?x) id=   (?: ("|')([\w\ ]+?)\1 | (\w+) )""".r
  val tagRegex   = """\<([a-zA-Z]\w*?)\W""".r
  def classesAndTags(txt: String): Set[String] = {
    val classes = classRegex.findAllMatchIn(txt).map { m => if (m.group(2) != null) m.group(2) else m.group(3) }.flatMap(_.split("\\s+"))
    val ids     = idRegex   .findAllMatchIn(txt).map { m => if (m.group(2) != null) m.group(2) else m.group(3) }.flatMap(_.split("\\s+"))
    val tags    = tagRegex.findAllMatchIn(txt).map(_.group(1))
    (tags ++ classes.map("."+_) ++ ids.map("#"+_)).toSet
  }

  def resolveGlobalLink(link: String, base: Base) = link match {
    case l if isAbsolute(l) => l
    case l if base.isValidId(l) => blog.absUrlFromSlug(base.canonicSlug(l))
    case l if l.contains('.') => l // rss.xml, index.html
    case "index" => blog.baseUrl+"/."
    case l => blog.absUrlFromSlug(l)
  }

  def rssLink(rss: String) =
    s"""<link rel="alternate" type="application/rss+xml" href="${rel(rss)}"/>"""

  def ogTags(a: Article): String =
    if (blog.hasOgTags) {
      val mainImg  = mainImageUrl(a)
      val otherImg = otherImageUrl(a)
      val (tpe, img) = if (mainImg != null) ("summary_large_image", mainImg) else ("summary", otherImg)

      <meta name="twitter:card" content={tpe}/> + "" +
      <meta property="og:type" content="article"/> +
      <meta property="og:url" content={baseUrl}/> +
      <meta property="og:title" content={a.title}/> +
      <meta property="og:description" content={truncate(plaintextDescription(a), 200)}/> +
      ifs(img, <meta property="og:image" content={img}/>.toString) +
      ifs(blog.twitterSite,    <meta name="twitter:site" content={blog.twitterSite}/>.toString) +
      ifs(blog.twitterCreator, <meta name="twitter:creator" content={blog.twitterCreator}/>.toString)
    } else ""

  def makePage(content: String, title: String = null, containImages: Boolean = false, headers: String = null, includeCompleteStyle: Boolean = false): String = {
    def defaultHeader = s"""<div class=r><b><a href="index">${blog.title}</a></b> [<a href="rss.xml">RSS</a>]</div>"""
    val protoHeader = if (blog.header.nonEmpty) blog.header else defaultHeader
    val header = ahrefRegex.replaceAllIn(protoHeader, m => Regex.quoteReplacement(rel(resolveGlobalLink(m.group(1), base))))
    val footer = ahrefRegex.replaceAllIn(blog.footer, m => Regex.quoteReplacement(rel(resolveGlobalLink(m.group(1), base))))
    val c1 = __ahrefRegex.replaceAllIn(content, { l =>
      if (l.group(2) == blog.invalidLinkMarker) Regex.quoteReplacement(l.group(4))
      else Regex.quoteReplacement(l.group(1)+rel(l.group(2))+l.group(3)+l.group(4)+l.group(5))
    })
    val c2 = if (!c1.contains(imgsrcCheck)) c1 else imgsrcRegex.replaceAllIn(c1,     l => Regex.quoteReplacement(rel(l.group(1))))
    val body = "<body><div class=b>"+header+c2+footer+"</div></body>"
    val cats: Set[String] = if (includeCompleteStyle) null else classesAndTags(body)


s"""<!DOCTYPE html>
<html${ifs(blog.hasOgTags, " prefix=\"og: http://ogp.me/ns#\"")}>
<head>
<meta charset="utf-8" />
<title>${ifs(title, title+" | ")+blog.title}</title>
${rssLink("rss.xml")}
${ifs(headers)}
${if (blog.cssFile.isEmpty) { s"""<style>${layoutMill.style(cats)}</style>""" }
  else { s"""<link rel="stylesheet" href="${rel("style.css")}" type="text/css"/>""" } }
${ifs(containImages, s"<script>$galleryScript</script>")}
</head>$body
</html>"""
  }

  def makeIndex(fullArticles: Seq[Article], links: Seq[Article], archiveLinks: Seq[Article] = Seq(), groupArchiveByMonth: Boolean = false): String =
    ifs(blog.scripts.indexPrepend != null && fullArticles.nonEmpty, eval(blog.scripts.indexPrepend, Map("articles" -> fullArticles)))+
    fullArticles.map(_makeFullArticle(_, true)).mkString("<br/><br/><br clear=all/>\n")+"<br/>"+
    listOfLinks(links, blog.archiveFormat == "short")+"<br/>"+
    (if (!groupArchiveByMonth) listOfLinks(archiveLinks, false) else groupArchive(archiveLinks))+"<br/>"

  private def groupArchive(archiveLinks: Seq[Article]) =
    "<div style='clear:both'>"+
    archiveLinks.groupBy(a => year(a.date)).toSeq.sortBy(_._1).reverse.map { case (y, as) =>
      val mas = if (y < year(new Date)) {
        (1 to 12).map { m => (m, as.find(a => month(a.date) == m)) }
      } else {
        as.reverse.map { a => (month(a.date), Some(a)) }
      }
      y+" "+mas.map {
        case (m, Some(a)) => articleLink(a, "&nbsp;"+m+"&nbsp;")
        case (m, None) => "&nbsp;"+m+"&nbsp;"
      }.mkString(" ")
    }.mkString("<br/>")+"</div>"

  def makeFullArticle(a: Article): String = _makeFullArticle(a, false)

  // Bare article body, no title, no links to related articles, no tags. This method is used in RSS feeds.
  def makeArticleBody(a: Article, compact: Boolean): String = {
    (if (blog.scripts.body != null) eval(blog.scripts.body, Map("body" -> a.text.render(this)))
    else a.text.render(this))+
    ifs(a.isTag, {
      val linked = a.slugsOfLinkedArticles(blog).toSet
      listOfLinks(base.allTags(a.asTag)._2.filter(a => !linked.contains(a.asSlug)), blog.tagFormat == "short")
    })+
    ifs(!compact, a.extraImages.map(img => imgTag(img.asSmallThumbnail, if (img.localSource != null) img.localSource.text else a.text)).mkString(" "))+
    ifs( compact, gallerySample(a))
  }

  def makeShortArticleBody(a: Article): String = {
    val img = a.images.sortBy(_.mods != "main").headOption.map(i => imgTag(i.asSmallThumbnail, a.text, false, blog.absUrl(a))).getOrElse("")
    val txt = truncate(plaintextDescription(a), 300)

    ifs(img, s"<div class=shimg>$img</div> ")+txt+" "+articleLink(a, txl("continueReading"))
  }

  def listOfLinks(list: Seq[Article], shortArticles: Boolean) =
    if (shortArticles) list.map(makeShortArticle).mkString+"<br/>&nbsp;"
    else               list.map(makeLink).mkString("<br/>")+"<br/>"

  def rowOfLinks(list: Seq[Article]) =
    list.map(a => articleLink(a, a.title)).mkString(", ")

  def makeShortArticle(a: Article): String = "<div class=sh>"+makeTitle(a)+"<br/>"+makeShortArticleBody(a)+"</div>"


  private def _makeFullArticle(a: Article, compact: Boolean): String = {
    (if (blog.scripts.title != null) eval(blog.scripts.title, Map("article" -> a, "compact" -> compact)) else
    (if(!a.isTag) makeTitle(a, compact) else txl("tagged")+" "+makeTitle(a)+"<br/>"))+
    ifs(!compact, "<span class=f>"+makeNextPrevArrows(a)+"</span>")+
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
    ifs(blog.scripts.title == null, "<br/>\n")+
    makeArticleBody(a, compact)+
    ifs(!compact && blog.scripts.fullArticleBottom != null, eval(blog.scripts.fullArticleBottom, Map("article" -> a)))+
    ifs(!compact,
      """<div class=low>"""+
      ifs(blog.allowComments && !a.isTag, s"""<hr/><b><a href="comments.php?url=${blog.relUrlFromSlug(a.slug)}">${txl("comments.enter")}</a></b>""")+
      ifs(blog.shareLinks && !a.isTag, {
        val url = URLEncoder.encode(blog.absUrl(a), "UTF-8")
        s""" &nbsp;&nbsp; ${txl("share.share")} <a href="https://www.facebook.com/sharer/sharer.php?u=$url">${txl("share.facebook")}</a>, <a href="https://twitter.com/intent/tweet?url=$url">${txl("share.twitter")}</a>, <a href="https://plus.google.com/share?url=$url">${txl("share.googleplus")}</a>"""
      })+
      "<hr/>"+
      """<div class="f r" style="max-width:50%">"""+
        ifs(a.tags.visible.nonEmpty, makeTagLinks(a.tags.visible.sortBy(!_.supertag).map(base.tagByTitle), a)+"<br/>\n")+
        ifs(a.license, a.license+"<br/>")+
      "</div>"+
      "<p>"+makeNextPrevLinks(a)+"</p>"+
      ifs(a.pubAricles.nonEmpty, txl("published")+"<br/>"+ a.pubAricles.map(makeLink).mkString("<br/>")+"<br/>")+
      ifs(a.pubBy != null, txl("publishedBy")+" "+articleLink(a.pubBy, makeDate(a), allowImageMarker = true)+"<br/>")+
      ifs(a.similar.nonEmpty,   "<p>"+txl("similar")+"<br/>"  +a.similar.map(s => articleLink(s, s.title, allowImageMarker = true)).mkString("<br/>")  +"</p>")+
      ifs(a.backlinks.nonEmpty, "<p>"+txl("backlinks")+"<br/>"+a.backlinks.map(s => articleLink(s, s.title, allowImageMarker = true)).mkString("<br/>")+"</p>")+
      "</div>"
    )
  }

  def makeTagIndex(base: Base) =
    base.allTags.toSeq.sortBy { case (_, (t, as)) => (~as.size, t.slug) }.map { case (_, (t, as)) => makeTagLink(t)+" ("+as.size+")" }.mkString(" ")

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

  def articleUrl(a: Article) = if (a.link != null) a.link else blog.absUrl(a)

  def articleLink(a: Article, title: String, asLink: Boolean = true, allowImageMarker: Boolean = false) =
    if (a.link != null || asLink)
      s"""<i><a href="${articleUrl(a)}">$title</a></i>"""+ifs(allowImageMarker && a.hasImageMarker, blog.imageMarker)
    else
      s"""<i>$title</i>"""+ifs(allowImageMarker && a.hasImageMarker, blog.imageMarker)

  def makeLink(a: Article) = makeDate(a)+" "+articleLink(a, a.title, allowImageMarker = true)

  def makeTagLink(t: Article) = {
    val html = s"""#<i><a href="${blog.absUrlFromSlug(t.slug)}">${t.title}</a></i>"""
    if (t.isSupertag) s"<b>$html</b>" else html
  }

  def makeNextPrevLinks(a: Article) =
    ifs(base.prev(a), "««« "+makeLink(base.prev(a))+"<br/>") +
    ifs(base.next(a), "»»» "+makeLink(base.next(a))+"<br/>")

  def makeNextPrevArrows(a: Article) = {
    val (prev, next) = (base.prev(a), base.next(a))
    if (prev == null && next == null) "" else  _makeNextPrevArrows(prev, next)
  }

  def _makeNextPrevArrows(prev: Article, next: Article) =
    (if (prev == null) "«««" else s"""<a href="${blog.absUrl(prev)}">«««</a>""")+" "+
    (if (next == null) "»»»" else s"""<a href="${blog.absUrl(next)}">»»»</a>""")

  def makeTagLinks(tags: Seq[Article], a: Article = null) =
    tags.map { t =>
      makeTagLink(t) + ifs(t.isSupertag && a != null,
        ifs(base.prev(a, t.asTag), s""" <a href="${blog.absUrl(base.prev(a, t.asTag))}">««</a>""")+
        ifs(base.next(a, t.asTag), s""" <a href="${blog.absUrl(base.next(a, t.asTag))}">»»</a>""")
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
