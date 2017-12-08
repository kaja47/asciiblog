package asciiblog

import MakeFiles. { Blog, relativize, year, month, isAbsolute, absUrl, absUrlFromSlug, absUrlFromPath, relUrlFromSlug, bigThumbnailUrl, thumbnailUrl, galleryScript }
import AsciiText. { ahrefRegex, ahrefCheck, imgsrcRegex, imgsrcCheck }
import java.text.SimpleDateFormat
import java.util.Date
import scala.util.matching.Regex



trait Layout extends ImageLayout {
  def makePage(content: String, title: String = null, containImages: Boolean = false, headers: String = null, includeCompleteStyle: Boolean = false): String
  def makeIndex(fullArticles: Seq[Article], links: Seq[Article], archiveLinks: Seq[Article] = Seq(), groupArchiveByMonth: Boolean = false, prev: Article = null, next: Article = null): String
  def makeFullArticle(a: Article): String
  def makeTagIndex(base: Base): String
}

// this type is passed into markup
trait ImageLayout {
  def imgTag(img: Image, t: Text, showDesc: Boolean = true): String
}

object FlowLayout {
  def stripTags(html: String, except: Seq[String] = Seq()) = {
    val exceptRegex = (if (except.isEmpty) "" else "(?!"+except.mkString("|")+")")
    html.replaceAll(s"\\<$exceptRegex.*?\\>", "") // TODO less crude way to strip tags
  }
  def truncate(txt: String, len: Int, append: String = "\u2026"): String =
    if (txt.length <= len) txt else txt.take(len).replaceAll("""\s+(\w+)?$|\<\w+$""", "")+append
  def updateLinks(content: String, f: String => String) =
    ahrefRegex.replaceAllIn(content, m => Regex.quoteReplacement(f(m.group(1))))
}


case class FlowLayout(baseUrl: String, base: Base, blog: Blog.type, markup: Markup) extends Layout {
  import FlowLayout._

  def rel(url: String): String =
    if (baseUrl == null || url.startsWith("#")) url
    else relativize(url, baseUrl)

  def txl(s: String) = blog.translation(s)
  def ifs(c: Boolean, body: => String) = if (c) body else ""
  def ifs(x: String, body: => String) = if (x != null && x.nonEmpty) body else ""
  def ifs(x: Any, body: => String) = if (x != null) body else ""
  def ifs(x: String) = if (x != null) x else ""

  private def plaintextDescription(a: Article): String =
    stripTags(a.text.firstParagraph).replaceAll("\\n", " ")

  private def articleImages(a: Article): Seq[Image] = a.images
  private def mainImageUrl(a: Article): String  = articleImages(a).find(_.mods == "main").map(_.url).getOrElse(null)
  private def otherImageUrl(a: Article): String = articleImages(a).map(_.url).headOption.getOrElse(null)

  def imgTag(img: Image, t: Text, showDesc: Boolean = true) = {
    val (cl, srcPath) = img match {
      case i if i.mods == "main" && i.align == ">" => ("fr", bigThumbnailUrl(img, true))
      case i if i.mods == "main" => ("main", bigThumbnailUrl(img, false))
      case i => ("thz", thumbnailUrl(img))
    }
    val desc = if (showDesc) {
      val title   = ifs(img.title, t.paragraph(img.title)).trim
      val tags    = makeTagLinks(img.tags.visible.map(base.tagByTitle)).trim
      val source  = ifs(img.source, s"""(<a href="${img.source}">${txl("source")}</a>)""")
      val license = (ifs(img.license)+" "+source).trim
      val locSrc  = ifs(img.localSource, articleLink(img.localSource, img.localSource.title))
      Seq(title, tags, license, locSrc).mkString(" ").replaceAll(" +", " ").trim
    } else ""
    s"""<span class=$cl><a href="${img.url}"><img class=thz ${ifs(img.alt, s"title='${img.alt}' ") }src="${absUrlFromPath(srcPath)}"/></a>$desc</span>"""
  }

  def gallerySample(a: Article) =
    a.extraImages.take(3).map { i =>
      s"""<a href="${relUrlFromSlug(a.slug)}"><img class=th src="${thumbnailUrl(i)}"/></a>"""
    }.mkString(" ")

  val classRegex = """(?x) class=(?: ("|')([\w\ ]+?)\1 | (\w+) )""".r

  val tagRegex   = """\<([a-zA-Z]\w*?)\W""".r
  def classesAndTags(txt: String): Set[String] = {
    val classes = classRegex.findAllMatchIn(txt).map { m => if (m.group(2) != null) m.group(2) else m.group(3) }.flatMap(_.split("\\s+"))
    val tags    = tagRegex.findAllMatchIn(txt).map(_.group(1))
    (tags ++ classes.map("."+_)).toSet
  }

  val selectorRegex = """^(\w+)?(\.\w+)?$""".r
  def style(styleLine: String, cats: String => Boolean): Option[String] = {
    val (selectorList, rule) = styleLine.span(_ != '{')
    val matchingSelectors = selectorList.split(",").map(_.trim).flatMap { selector =>
      val matches = selector.split(" ").forall { s =>
        selectorRegex.findFirstMatchIn(s) match {
          case Some(m) => (Option(m.group(1)) ++ Option(m.group(2))).forall(cats)
          case None => true
        }
      }
      if (matches) Some(selector) else None
    }
    if (matchingSelectors.nonEmpty) Some(matchingSelectors.mkString(",")+rule) else None
  }

  def styles(styleTxt: String, cats: String => Boolean) =
    styleTxt.lines.map(_.trim).filter(_.nonEmpty).flatMap(l => style(l, cats)).mkString("")

  def resolveGlobalLink(link: String, base: Base) = link match {
    case l if isAbsolute(l) => l
    case l if base.isValidId(l) => absUrlFromSlug(base.canonicSlug(l))
    case l if l.contains('.') => l // rss.xml, index.html
    case l => absUrlFromSlug(l)    // index or just slug
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
    val defaultHeader = s"""<div class=r><b><a href="index">${blog.title}</a></b> [<a href="rss.xml">RSS</a>]</div>"""
    val protoHeader = if (blog.header.nonEmpty) blog.header else defaultHeader
    val header = ahrefRegex.replaceAllIn(protoHeader, m => Regex.quoteReplacement(rel(resolveGlobalLink(m.group(1), base))))
    val footer = ahrefRegex.replaceAllIn(blog.footer, m => Regex.quoteReplacement(rel(resolveGlobalLink(m.group(1), base))))
    val c1 = ahrefRegex.replaceAllIn(content, l => Regex.quoteReplacement(rel(l.group(1))))
    val c2 = if (!c1.contains(imgsrcCheck)) c1 else imgsrcRegex.replaceAllIn(c1,     l => Regex.quoteReplacement(rel(l.group(1))))
    val body = "<body><div class=b>"+header+c2+footer+"</div></body>"
    val cats: String => Boolean = if (includeCompleteStyle) _ => true else classesAndTags(body)

    def inlineStyles = styles(s"""
a{color:inherit}
.r{text-align:right}
.f{float:right}
.b{max-width:46em;font-family:monospace;line-height:1.3}
blockquote{margin:0;padding:0;font-style:italic}
.about{text-decoration:underline red}
.thz,.fr,.main{font-size:0.8em}
span.thz {width:${blog.thumbWidth}px;display:inline-block;vertical-align:top}
span.fr {text-align:right;max-width:45%;float:right}
span.main {text-align:right;display:block;margin-bottom:0.5em}
span.main img, span.fr img {max-width:100%}
h2 {display:inline;margin:0;font-size:1em}
hr {border:0px dotted gray;border-top-width:1px;margin:0.8em 4em}
p {margin:1.4em 0}
.sh {float:left;clear:both;margin:0.7em 0}
.shimg {float:left;margin:0 0.5em 0 0}
""", cats)+blog.cssStyle

s"""<!DOCTYPE html>
<html${ifs(blog.hasOgTags, " prefix=\"og: http://ogp.me/ns#\"")}>
<head>
<meta charset="utf-8" />
<title>${ifs(title, title+" | ")+blog.title}</title>
${rssLink("rss.xml")}
${ifs(headers)}
${if (blog.cssFile.isEmpty) { s"""<style>$inlineStyles</style>""" }
  else { s"""<link rel="stylesheet" href="${rel("style.css")}" type="text/css"/>""" } }
${ifs(containImages, s"<script>$galleryScript</script>")}
</head>$body
</html>"""
  }

  def makeIndex(fullArticles: Seq[Article], links: Seq[Article], archiveLinks: Seq[Article] = Seq(), groupArchiveByMonth: Boolean = false, prev: Article = null, next: Article = null): String =
    "<span class=f>"+_makeNextPrevArrows(prev, next)+"</span>"+
    fullArticles.map(_makeFullArticle(_, true)).mkString("<br/><br/><br clear=all/>\n")+"<br/>"+
    listOfLinks(links, blog.archiveFormat == "short")+"<br/>"+
    (if (!groupArchiveByMonth) listOfLinks(archiveLinks, false) else groupArchive(archiveLinks))+"<br/>"+
    "<span class=f>"+_makeNextPrevArrows(prev, next)+"</span>"

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
    a.text.render(this)+
    ifs(a.isTag, {
      val linked = a.slugsOfLinkedArticles.toSet
      listOfLinks(base.allTags(a).filter(a => !linked.contains(a.asSlug)), blog.tagFormat == "short")
    })+
    ifs(!compact, a.extraImages.map(img => imgTag(img.asSmallThumbnail, if (img.localSource != null) img.localSource.text else a.text)).mkString(" "))+
    ifs( compact, gallerySample(a))
  }

  def makeShortArticleBody(a: Article): String = {
    val img = articleImages(a).sortBy(_.mods != "main").headOption.map(i => imgTag(i.asSmallThumbnail, a.text, false)).getOrElse("")
    val txt = truncate(stripTags(a.text.render(this), Seq("wbr")), 300)
    ifs(img, s"<div class=shimg>$img</div> ")+txt+" "+articleLink(a, txl("continueReading"))
  }

  def listOfLinks(list: Seq[Article], shortArticles: Boolean) =
    if (shortArticles) list.map(makeShortArticle).mkString+"<br/>&nbsp;"
    else               list.map(makeLink).mkString("<br/>")+"<br/>"

  def rowOfLinks(list: Seq[Article]) =
    list.map(a => articleLink(a, a.title)).mkString(", ")

  def makeShortArticle(a: Article): String = "<div class=sh>"+makeTitle(a)+"<br/>"+makeShortArticleBody(a)+"</div>"


  private def _makeFullArticle(a: Article, compact: Boolean): String = {
    (if(!a.isTag) makeTitle(a, compact) else txl("tagged")+" "+makeTitle(a)+"<br/>")+
    ifs(!compact, "<span class=f>"+makeNextPrevArrows(a)+"</span>")+
    ifs(a.isTag, {
      val sup = a.tags.visible.map(base.tagByTitle)
      val sub = base.allTags(a).filter(_.isTag)
      ifs(sub.nonEmpty || sup.nonEmpty, {
        "<div style='font-size:0.85em'>"+
        ifs(sup.nonEmpty, txl("supersections")+" "+rowOfLinks(sup)+" ")+
        ifs(sub.nonEmpty, txl("subsections")  +" "+rowOfLinks(sub))+
        "</div>"
      })
    })+
    "<br/>\n"+
    makeArticleBody(a, compact)+
    ifs(!compact,
      ifs(blog.allowComments && !a.isTag, s"""<hr/><b><a href="comments.php?url=${relUrlFromSlug(a.slug)}">${txl("comments.enter")}</a></b> """)+
      "<hr/>"+
      """<div style="font-size:0.9em;">"""+
      """<div class="f r" style="max-width:50%">"""+
        ifs(a.tags.visible.nonEmpty, makeTagLinks(a.tags.visible.sortBy(!_.supertag).map(base.tagByTitle), a)+"<br/>\n")+
        ifs(a.license, a.license+"<br/>")+
      "</div>"+
      "<p>"+makeNextPrevLinks(a)+"</p>"+
      ifs(a.pub.nonEmpty, txl("published")+"<br/>"+ a.pub.map(makeLink).mkString("<br/>")+"<br/>")+
      ifs(a.pubBy != null, txl("publishedBy")+" "+articleLink(a.pubBy, makeDate(a))+"<br/>")+
      ifs(a.similar.nonEmpty,   "<p>"+txl("similar")+"<br/>"  +a.similar.map(s => articleLink(s, s.title)).mkString("<br/>")  +"</p>")+
      ifs(a.backlinks.nonEmpty, "<p>"+txl("backlinks")+"<br/>"+a.backlinks.map(s => articleLink(s, s.title)).mkString("<br/>")+"</p>")+
      "</div>"
    )
  }

  def makeTagIndex(base: Base) =
    base.allTags.toSeq.sortBy(~_._2.size).map { case (t, as) => makeTagLink(t)+" ("+as.size+")" }.mkString(" ")

  def makeDateWithLinkToPubBy(a: Article, asLink: Boolean) =
    if (a.pubBy != null && !asLink) articleLink(a.pubBy, makeDate(a))
    else makeDate(a)

  def makeTitle(a: Article, asLink: Boolean = true) =
    makeDateWithLinkToPubBy(a, asLink)+" <h2>"+articleLink(a, a.title, asLink)+"</h2>"

  def articleLink(a: Article, title: String, asLink: Boolean = true) =
    if (a.link != null || asLink)
      s"""<i><a href="${if (a.link != null) a.link else absUrl(a)}">$title</a></i>"""+ifs(a.hasImageMarker, blog.imageMarker)
    else
      s"""<i>$title</i>"""+ifs(a.hasImageMarker, blog.imageMarker)

  def makeLink(a: Article) = makeDate(a)+" "+articleLink(a, a.title)

  def makeTagLink(t: Article) = {
    val html = s"""#<i><a href="${absUrlFromSlug(t.slug)}">${t.title}</a></i>"""
    if (t.isSupertag) s"<b>$html</b>" else html
  }

  def makeNextPrevLinks(a: Article) =
    ifs(base.prev(a), "««« "+makeLink(base.prev(a))+"<br/>") +
    ifs(base.next(a), "»»» "+makeLink(base.next(a))+"<br/>")

  def makeNextPrevArrows(a: Article) = _makeNextPrevArrows(base.prev(a), base.next(a))

  def _makeNextPrevArrows(prev: Article, next: Article) =
    (if (prev == null) "&nbsp;&nbsp;&nbsp;" else s"""<a id=prev href="${absUrl(prev)}">«««</a>""")+" "+
    (if (next == null) "&nbsp;&nbsp;&nbsp;" else s"""<a id=next href="${absUrl(next)}">»»»</a>""")

  def makeTagLinks(tags: Seq[Article], a: Article = null) =
    tags.map { t =>
      makeTagLink(t) + ifs(t.isSupertag && a != null,
        ifs(base.prev(a, t.asTag), s""" <a href="${absUrl(base.prev(a, t.asTag))}">««</a>""")+
        ifs(base.next(a, t.asTag), s""" <a href="${absUrl(base.next(a, t.asTag))}">»»</a>""")
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
