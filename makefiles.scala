import java.util.{ Date, GregorianCalendar, Calendar, Locale }
import java.text.SimpleDateFormat
import java.io.File
import java.net.URL
import java.awt.image.{ BufferedImage, ConvolveOp, Kernel }
import java.awt.{ AlphaComposite, RenderingHints => RH }
import javax.imageio.ImageIO
import scala.collection.mutable


val cfg = io.Source.fromFile(args(0))
  .getLines
  .map(l => l.split(" ", 2))
  .map { case Array(k, v) => (k, v)}
  .toMap

  val galleryScript = {
    val thisFile = System.getProperty("sun.java.command").split(" ").find(x => x.endsWith("makefiles.scala")).get
    val scriptFile = new File(thisFile).getParent+"/gallery.js"
    io.Source.fromFile(scriptFile).mkString
      .replaceAll("(?<!let|function|in)[\\s]+(?!in)|/\\*.*?\\*/", "") // rather crude and incorrect minifier
  }

object Blog {
  val kind: String         = cfg.getOrElse("type", "blog")
  val title: String        = cfg("title")
  val baseUrl: String      = cfg("baseUrl")
  val articlesOnIndex: Int = cfg("fullArticlesOnIndex").toInt
  val style: String        = cfg.getOrElse("style", "")
  val thumbWidth: Int      = cfg.getOrElse("thumbnailWidth", "150").toInt
  val thumbHeight: Int     = cfg.getOrElse("thumbnailHeight", "100").toInt
  val limitRss: Int        = cfg.getOrElse("limitRss", Int.MaxValue.toString).toInt
  val sortByDate: Boolean  = cfg.getOrElse("sortByDate", "false").toBoolean

  val articlesMustBeSorted: Boolean = cfg.getOrElse("articlesMustBeSorted", "true").toBoolean
  val articlesMustNotBeMixed: Boolean = cfg.getOrElse("articlesMustNotBeMixed", "true").toBoolean
}

case class Article(
  title: String,
  slug: String,
  date: Date,
  tags: Tags = Tags(),
  license: String = null,
  rawText: String,
  images: Seq[Image] = Seq(),
  linkMap: Map[String, String] = Map(),
  backlinks: Seq[Article] = Seq(),
  similar: Seq[Article] = Seq(),
  prev: Article = null,
  next: Article = null
) {
  def links: Seq[String] = linkMap.values.toVector
  override def toString = {
    val f = new SimpleDateFormat("MM-dd-yyyy")
    "Article("+(if (date == null) "" else f.format(date)+" ")+title+")"
  }
}

case class Tags(visible: Seq[String] = Seq(), hidden: Seq[String] = Seq(), supertags: Seq[String] = Seq())
case class Image(url: String, thumb: String)
case class Sim(article: Article, commonTags: Int)



def resizeImage(src: BufferedImage, width: Int, height: Int): BufferedImage = {
  val zoom = math.min(1.0 * src.getWidth / width, 1.0 * src.getHeight / height)
  val wz = (width * zoom).toInt
  val hz = (height * zoom).toInt
  val x = (src.getWidth - wz) / 2
  val y = (src.getHeight - hz) / 2

  progressiveResize(src.getSubimage(x, y, wz, hz), width, height)
}

/** adapted from https://github.com/coobird/thumbnailator/blob/master/src/main/java/net/coobird/thumbnailator/resizers/ProgressiveBilinearResizer.java */
def progressiveResize(src: BufferedImage, width: Int, height: Int): BufferedImage = {
  val dest = new BufferedImage(width, height, src.getType)

  var (currentWidth, currentHeight) = (src.getWidth, src.getHeight)

  if ((width * 2 >= currentWidth) && (height * 2 >= currentHeight)) {
    val g = dest.createGraphics()
    g.drawImage(src, 0, 0, width, height, null)
    g.dispose()
    return dest
  }

  val tmp = new BufferedImage(currentWidth, currentHeight, dest.getType)

  val g = tmp.createGraphics()
  g.setRenderingHint(RH.KEY_INTERPOLATION, RH.VALUE_INTERPOLATION_BILINEAR)
  g.setComposite(AlphaComposite.Src)

  var (startWidth, startHeight) = (width, height)

  while (startWidth < currentWidth && startHeight < currentHeight) {
    startWidth *= 2
    startHeight *= 2
  }

  currentWidth = startWidth / 2
  currentHeight = startHeight / 2

  g.drawImage(src, 0, 0, currentWidth, currentHeight, null)

  while ((currentWidth >= width * 2) && (currentHeight >= height * 2)) {
    currentWidth /= 2
    currentHeight /= 2

    if (currentWidth < width) { currentWidth = width }
    if (currentHeight < height) { currentHeight = height }

    g.drawImage(
        tmp,
        0, 0, currentWidth, currentHeight,
        0, 0, currentWidth * 2, currentHeight * 2,
        null
    )
  }

  g.dispose()

  val destg = dest.createGraphics()
  destg.drawImage(tmp, 0, 0, width, height, 0, 0, currentWidth, currentHeight, null)
  destg.dispose()

  dest
}



def absUrlFromPath(path: String) = Blog.baseUrl + "/" + path
def absUrlFromSlug(slug: String) = Blog.baseUrl + "/" + slug + ".html"
def relUrlFromSlug(slug: String) = slug + ".html"
//def relativize(url: String) = if (localLink(url)) dropLocalPrefix(url) else url
def relativize(url: String, baseUrl: String) = {
  import java.net.URI
  val u = new URI(url)
  val b = new URI(baseUrl)

  require(b.isAbsolute)

  if (u.getHost != null && u.getHost != b.getHost) {
    url

  } else {
    val us = u.getPath.split("/").filter(_.nonEmpty)
    val bs = b.getPath.split("/").filter(_.nonEmpty)

    val prefixLen = (us zip bs).takeWhile { case (u, b) => u == b }.length

    val backLevels = bs.length - prefixLen - 1
    (Seq.fill(backLevels)("..") ++ us.drop(prefixLen)).mkString("/")
  }
}
def localLink(url: String) = url.startsWith(Blog.baseUrl+"/")
def dropLocalPrefix(url: String) = url.drop(Blog.baseUrl.length+1)
def extractSlug(url: String) = if (localLink(url)) dropLocalPrefix(url).dropRight(5) else sys.error("not local url")

def thumbnailUrl(img: Image) = s"t/${img.thumb}-${Blog.thumbWidth}x${Blog.thumbHeight}"


def getArticle(lines: Vector[String]): (Article, Vector[String]) = {
  val underlinePos = lines.indexWhere(l => l.startsWith("==="), 2)

  if (underlinePos == -1) {
    (parseArticle(lines), Vector())
  } else {
    val (art, rest) = lines.splitAt(underlinePos-1)
    (parseArticle(art), rest)
  }
}


val titleRegex    = """^(.+?)(?:\[([^ ]+)\])?$""".r
val linkRefBlockRegex = """(?xm) (?:  ^\[(.*?)\]:\ (.+)\n  )*  ^\[(.*?)\]:\ (.+) """.r
val linkRefRegex      = """(?xm)      ^\[(.*?)\]:\ (.+)$""".r
val dateRegex     = """^(\d+)-(\d+)-(\d+)(?: (\d+):(\d+)(?::(\d+))?)?$""".r
val tagsRegex     = """^#(.+)$""".r
val supertagRegex = """^!(.+)$""".r

val boldRegex     = """(?xs)\*\*(.+?)\*\*""".r
val italicRegex   = """(?xs)(?<!\*)\*(?!\*)(.+?)(?<!\*)\*(?!\*)""".r
val linkRegex     = """(?x) " ([^"]+(?:\R[^"]+)?) " : \[ (\w+) \]""".r
val imgBlockRegex = """(?xm) (?: ^\[\*\ + [^* ]+ \ +\*\]\ *\n)+   (?: ^\[\*\ + [^* ]+ \ +\*\]\ *\n?) """.r
val imgRegex      = """(?xm)      \[\*\ +([^* ]+)\ +\*\]\ * """.r
val blockquoteRegex = """(?xm) ( (?: ^>[^\n]*\n)+ )""".r
val blackoutRegex = """(?xs) \[\|.+?\|\] """.r

def parseDate(l: String): Option[Seq[Date]] = {
  val dates = l.split(",").map(_.trim) map {
    case dateRegex(y, m, d, null, null, null) =>
      Some(new GregorianCalendar(y.toInt, m.toInt-1, d.toInt).getTime)
    case dateRegex(y, m, d, h, mi, null) =>
      Some(new GregorianCalendar(y.toInt, m.toInt-1, d.toInt, h.toInt, mi.toInt, 0).getTime)
    case dateRegex(y, m, d, h, mi, s) =>
      Some(new GregorianCalendar(y.toInt, m.toInt-1, d.toInt, h.toInt, mi.toInt, s.toInt).getTime)
    case _ => None
  }
  if (dates.nonEmpty && dates.forall(_.isDefined)) Some(dates.map(_.get)) else None
}

def parseLicense(l: String): Option[String] = {
  val ls = Set("CC by", "CC by-nc", "CC by-nd", "CC by-sa", "CC by-nc-nd", "CC by-nc-sa")
  if (ls.contains(l)) Some(l) else None
}

def parseTags(l: String): Option[Tags] = l match {
  case tagsRegex(ts) =>
    val (hidden, visible) = ts.split(",").map(_.trim).partition(t => t.startsWith("(") && t.endsWith(")"))
    Some(Tags(visible = visible, hidden = hidden.map(_.drop(1).dropRight(1))))
  case _ => None
}

def parseSupertags(l: String): Option[Tags] = l match {
  case supertagRegex(ts) =>
    Some(Tags(supertags = ts.split(",").map(_.trim)))
  case _ => None
}

def parseMeta(l: String): Option[String] = {
  val prefix = "meta: "
  if (l.startsWith(prefix)) Some(l.drop(prefix.length))
  else None
}

def parseRel(l: String): Option[String] = {
  val prefix = "rel: "
  if (l.startsWith(prefix)) Some(l.drop(prefix.length))
  else None
}

def process[T](ls: Seq[String], f: String => Option[T]): (Option[T], Seq[String]) = {
  val pairs = ls.zip(ls.map(f))
  val idx = pairs.indexWhere { case (l, v) => v != None }
  if (idx == -1) (None, ls) else {
    (pairs(idx)._2, ls.patch(idx, Seq(), 1))
  }
}

def hash(txt: String) = {
  val md5 = java.security.MessageDigest.getInstance("MD5")
  md5.reset()
  val digest = md5.digest(txt.getBytes("utf-8"))
  val bigInt = new java.math.BigInteger(1, digest)
  bigInt.toString(16).reverse.padTo(32, '0').reverse
}

def parseArticle(lines: Vector[String]): Article = {
  val ls = lines.map(_.replaceAll("\\s*$", ""))

  val titleLine = ls(0)
  val titleRegex(title, slug) = titleLine

  val (metaLines, _body) = ls.drop(2).span(l => l.nonEmpty)
  val body = _body.dropWhile(l => l.isEmpty).reverse.dropWhile(l => l.isEmpty).reverse


  def p[T, R](previous: (R, Seq[String]), f: String => Option[T]): ((R, Option[T]), Seq[String]) = {
    val (r, lines) = previous
    val (t, rest) = process(lines, f)
    ((r, t), rest)
  }

  val ((((((dates, tags), stags), license), meta), rel), rest) =
    p(p(p(p(p(process(metaLines, parseDate), parseTags), parseSupertags), parseLicense), parseMeta), parseRel)



  // meta ???

  if (rest.nonEmpty) sys.error("some metainformation was not processed: "+rest)

  val linkMap = body.collect {
    case linkRefRegex(r, url) => (r, url)
  }.toMap

  val images = body.collect { case imgRegex(url) => new Image(url, hash(url)) }

  new Article(
    title   = title.trim,
    slug    = if (slug == null || slug == "") generateSlug(title) else slug,
    date    = dates.map(_.head).getOrElse(null), // TODO, currently it's using only the first date
    tags    = tags.getOrElse(Tags()).copy(supertags = stags.getOrElse(Tags()).supertags),
    license = license.getOrElse(null),
    rawText = body.mkString("\n"),
    images  = images,
    linkMap = linkMap,
  )
}

def generateSlug(title: String) = {
  val from = "áčďéěíňóřšťúůýž"
  val to   = "acdeeinorstuuyz"
  val txl = (from zip to).toMap withDefault (x => x)

  title
    .toLowerCase
    .map(txl)
    .map{ ch => if (Character.isAlphabetic(ch) || Character.isDigit(ch)) ch else "-" }
    .mkString
    .replaceAll("--+", "-")
    .replaceAll("-+$", "")
    .replaceAll("^-+", "")
}

def tagSlug(title: String) = "tag-"+generateSlug(title)

def similarByTags(a: Article, tagMap: Map[String, Seq[Article]]): Seq[Sim] = {
  def dateDiff(a: Article, b: Article): Long = {
    (a.date, b.date) match {
      case (null, null) => 0
      case (null, _) | (_, null) => Long.MaxValue/2
      case (a, b) => math.abs(a.getTime - b.getTime)
    }
  }

  (a.tags.supertags ++ a.tags.visible ++ a.tags.hidden)
    .flatMap(t => tagMap.getOrElse(t, Seq()))
    .filter(_.slug != a.slug)
    .groupBy(identity)
    .map { case (a, ts) => Sim(a, ts.size) }
    .toVector
    .sortBy { s => (~s.commonTags, dateDiff(a, s.article)) } // most common tags, published closest together
}

def similarByTags(a: Article, tagMap: Map[String, Seq[Article]], without: Seq[Article]): Seq[Article] = {
  val bl = a.backlinks.toSet
  similarByTags(a, tagMap).filter(s => !bl.contains(s.article)).take(5).map(_.article)
}



trait Layout {
  def makePage(content: String, title: String = null, gallery: Boolean = false, rss: String = null): String
  def makeIndex(articles: Seq[Article]): String
  def makeFullArticle(a: Article, as: Seq[Article], prevNextNavigation: Boolean, tags: Boolean): String
  def makeTagPage(t: String, as: Seq[Article]): String
}

class FlowLayout(baseUrl: String) extends Layout {
  def undefRefError(ref: String, a: Article) = {
    println(s"link reference [$ref] is not defined in article '${a.title}'")
    ""
    //throw new Exception(s"link reference [$ref] is not defined in article '${a.title}'")
  }

  def rel(url: String): String = relativize(url, baseUrl)

  def decorateText(a: Article): String = {
    var txt = a.rawText
    txt = blackout(txt)
    txt = linkRegex.replaceAllIn(txt, m => {
      val ref = m.group(2)
      val url = rel(a.linkMap.getOrElse(ref, undefRefError(ref, a)))
      s"""<a href="${url}">${m.group(1)}</a>"""
    })
    txt = imageBlock(txt, a.images)
    txt = blockquoteRegex.replaceAllIn(txt, m =>
      "<blockquote>"+m.group(1).replaceAll("(?mx) ^\\>\\ +", "")+"</blockquote><br/>"
    )
    txt = txt.replaceAll("""(?xm) ^(-\ |\ \ )(.*?)(?=\n(?:-\ |\n\n)) """, "$1$2<br/>") // lists
    txt = linkRefBlockRegex.replaceAllIn(txt, "")
    txt = txt.replaceAll("\n*$", "")
    txt = txt.replaceAll("""(?xs) \<!--.*?--\>""", "")
    txt = txt.replaceAll("\n\n+|$", "<br/>\n<br/>\n")
    txt = boldRegex.replaceAllIn(txt, """<b>$1</b>""")
    txt = italicRegex.replaceAllIn(txt, """<i>$1</i>""")
    txt
  }

  def imageBlock(txt: String, images: Seq[Image]): String =
    imgBlockRegex.replaceAllIn(txt, m => {
      imgRegex.findAllMatchIn(m.group(0)).map { m =>
        val links = m.group(1)
        val thumbPath = thumbnailUrl(images.find(i => i.url == links).get)
        s"""<a href="$links"><img class=thz src="$thumbPath"/></a>"""
      }.mkString(" ")+"<br/>"
    })

  def makePage(content: String, title: String = null, gallery: Boolean = false, rss: String = null): String = {
s"""<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8" />
<title>${(if (title != null) title+" | " else "")+Blog.title}</title>
<link rel="alternate" type="application/rss+xml" href="${rel("rss.xml")}"/>
${if (rss != null) s"""<link rel="alternate" type="application/rss+xml" href="${rel(rss)}"/>""" else ""}
<style>a{color:inherit}blockquote{margin:0;padding:0;font-style:italic;}.r{text-align:right}.f{float:right}.b{max-width:46em;font-family:monospace}.th,.thz{width:${Blog.thumbWidth}px;height:${Blog.thumbHeight}px} ${Blog.style}</style>
${if (gallery) { "<script>"+galleryScript+"</script>" } else ""}
</head>
<body>
<div class=b>
<div class=r><b><a href="${rel("index.html")}">${Blog.title}</a></b> [<a href="${rel("rss.xml")}">RSS</a>]</div>
"""+content+"""
</div>
</body>
</html>"""
  }

  def makeIndex(articles: Seq[Article]): String = {
    val (fulls, links) = articles.splitAt(Blog.articlesOnIndex)
    (fulls.map(a => makeFullArticle(a, articles, false, false)) ++ links.map(makeLink)).mkString("<br/>\n") + "<br/>"
  }

  def makeFullArticle(a: Article, as: Seq[Article], prevNextNavigation: Boolean, tags: Boolean): String = {
    val sims = a.similar.take(5)

    makeLink(a)+
    (if (prevNextNavigation) "<span class=f>"+makeNextPrevArrows(a, as)+"</span>" else "")+
    "<br/><br/><br/>\n"+
    decorateText(a)+
    (if (prevNextNavigation) makeNextPrevLinks(a, as) else "")+{
      val rel =
        (if (tags && a.tags.supertags.nonEmpty) "<b>"+makeTagLinks(a.tags.supertags)+"</b><br/>" else "")+
        (if (tags && a.tags.visible.nonEmpty) makeTagLinks(a.tags.visible)+"<br/>" else "")+
        (if (tags && (a.backlinks.nonEmpty || sims.nonEmpty)) makeRelLinks(Seq(sims, a.backlinks))+"<br/>" else "")

      if (rel.nonEmpty) "<div class=r>"+rel+"</div>" else ""
    }+
    (if (a.license != null) "<div class=r>"+a.license+"</div>" else "")
  }

  def makeTagPage(t: String, as: Seq[Article]) = {
    makeTagLink(t)+"<br/><br/>"+
    as.map(makeLink).mkString("<br/>")
  }

  def blackout(txt: String) =
    blackoutRegex.replaceAllIn(txt, m => m.group(0).replaceAll("(?s).", "█").grouped(5).mkString("<wbr>"))

  def makeLink(a: Article) =
    makeDate(a)+s"""<i><a href="${rel(absUrlFromSlug(a.slug))}">${a.title}</a></i>"""

  def makeTagLink(t: String) =
    s"""<span class=y>#</span><i><a href="${rel(absUrlFromSlug(tagSlug(t)))}">${t}</a></i>"""

  def makeRelLink(a: Article, ord: Int) =
    s"""<i><a href="${rel(absUrlFromSlug(a.slug))}">#${ord}</a></i>"""

  def makeNextPrevLinks(a: Article, as: Seq[Article]) =
    (if (a.prev == null) "" else "&lt;&lt;&lt; "+makeLink(a.prev)+"<br/>") +
    (if (a.next == null) "" else "&gt;&gt;&gt; "+makeLink(a.next)+"<br/>")

  def makeNextPrevArrows(a: Article, as: Seq[Article]) =
    (if (a.prev == null) "&nbsp;&nbsp;&nbsp;" else s"""<a id=prev href="${rel(absUrlFromSlug(a.prev.slug))}">&lt;&lt;&lt;</a>""")+" "+
    (if (a.next == null) "&nbsp;&nbsp;&nbsp;" else s"""<a id=next href="${rel(absUrlFromSlug(a.next.slug))}">&gt;&gt;&gt;</a>""")

  def makeTagLinks(ts: Seq[String]) =
    ts.map(makeTagLink).mkString(" ")

  def makeRelLinks(ass: Seq[Seq[Article]]) =
    ass.map { as =>
      as.zipWithIndex.map { case (a, i) => makeRelLink(a, i+1) }.mkString(" ")
    }.mkString(" ")


  def year(d: Date) = {
    val calendar = new GregorianCalendar()
    calendar.setTime(d)
    calendar.get(Calendar.YEAR)
  }

  val thisYear = year(new Date)

  def makeDate(a: Article) =
    if (a.date == null) "" else {
      if (year(a.date) == thisYear) {
        new SimpleDateFormat("d. M.").format(a.date)+" "
      } else {
        new SimpleDateFormat("d. M. yyyy").format(a.date)+" "
      }
    }
}




def rssdate(date: Date) = if (date == null) "" else
  new SimpleDateFormat("EEE', 'dd' 'MMM' 'yyyy' 'HH:mm:ss' 'Z", Locale.US).format(date)

def generateRSS(articles: Seq[Article]): String = {
  "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n" + (
<rss version="2.0">
<channel>
<title>{Blog.title}</title>
{articles.take(Blog.limitRss).map { a =>
<item><title>{a.title}</title><guid isPermaLink="true">{absUrlFromSlug(a.slug)}</guid><pubDate>{rssdate(a.date)}</pubDate></item>
}}
</channel>
</rss>).toString
}



def saveFile(f: String, content: String): (String, String) = {
  val p = new File(f).getParentFile
  if (p != null) p.mkdirs()

  val fw = new java.io.FileWriter(f)
  fw.write(content)
  fw.close()
  (f, hash(content))
}

def saveXml(f: String, content: String): (String, String) =
  saveFile(f+".xml", content)




def prepareBlog(): (Seq[Article], Map[String, Seq[Article]]) = {
  var lines: Vector[String] = args.tail.flatMap { f =>
    io.Source.fromFile(f).getLines ++ Seq("\n","\n")
  }.toVector

  var articlesList = List[Article]()
  val today = new Date

  while (lines.nonEmpty) {
    val (a, ls) = getArticle(lines)
    articlesList ::= a
    lines = ls
  }

  var articles = articlesList.reverse.toVector

  if (Blog.articlesMustNotBeMixed) {
    val (hidden, rest1) = articles.span { a => a.title.startsWith("?") }
    val (visible, rest) = rest1.span { a => !a.title.startsWith("?") }
    if (rest.nonEmpty) sys.error("hidden and visible articles are mixed up")
  }

  articles = articles.filter { a =>
    val isInPast = a.date == null || a.date.before(today)
    !a.title.startsWith("?") && isInPast
  }

  // slug duplicates
  articles.groupBy(_.slug) foreach { case (slug, as) =>
    if (as.size > 1) {
      sys.error("multiple articles with the same slug '"+slug+"'")
    }
  }

  // ordered by date
  if (Blog.articlesMustBeSorted) {
    val ordered = articles
      .filter(_.date != null)
      .map(_.date)
      .sliding(2)
      .forall { case Seq(a, b) => a.compareTo(b) >= 0 }

    if (!ordered) sys.error("articles are not ordered by date")
  }

  if (Blog.sortByDate) { // newes first, articles without date last
    articles = articles.sortBy(a => ~(if (a.date == null) 0 else a.date.getTime))
  }

  def invert[A, B](m: Seq[(A, Seq[B])]): Map[B, Seq[A]] =
    m.flatMap { case (a, bs) => bs.map { b => (b, a) }  }
      .groupBy(_._1)
      .map { case (b, bas) => (b, bas.map(_._2).toVector) }

  val tagMap: Map[String, Seq[Article]] =
    invert(articles.map { a => (a, a.tags.visible) })

  val allTagMap: Map[String, Seq[Article]] =
    invert(articles.map { a => (a, a.tags.visible ++ a.tags.hidden) })

  val backlinks: Map[String, Seq[Article]] =
    invert(articles.map { a => (a, a.links filter localLink map extractSlug) })

  articles = articles.zipWithIndex map { case (a, i) =>
    val bs = backlinks.getOrElse(a.slug, Seq())

    a.copy(
      backlinks = bs,
      similar = similarByTags(a, allTagMap, without = bs),
      prev = if (a == articles.head) null else articles(i-1),
      next = if (a == articles.last) null else articles(i+1)
    )
  }

  (articles, tagMap)
}



def prepareGallery(): (Seq[Article], Seq[Article]) = {
  var dir = args(1)
  val albumDirs = new File(dir, "albums").listFiles.sortBy(_.getName).reverse

  val dateTitle = """^(?:(\d+)-(\d+)-(\d+)\s*-?\s*)?(.*)$""".r

  (for (albumDir <- albumDirs.toSeq) yield {
    println(albumDir.getName)

    val dateTitle(y, m, d, t) = albumDir.getName
    val (date, title) =
      if (y == null) (null, t)
      else (new GregorianCalendar(y.toInt, m.toInt-1, d.toInt).getTime, t)

    val urls = albumDir.list collect { case f if f.toLowerCase.endsWith(".jpg") =>
      Blog.baseUrl+"/albums/"+albumDir.getName+"/"+f
    }

    val imgs = urls.map { url => new Image(url, hash(url)) }

    def mkText(imgs: Seq[Image]) =
      imgs.map { i => s"[* ${i.url} *]" }.mkString("\n")+"\n"

    def mkSample(a: Article) =
      s"""<a href="${relUrlFromSlug(a.slug)}">"""+
      a.images.take(3).map { i =>
        s"""<img class=th src="${thumbnailUrl(i)}" />"""
      }.mkString(" ")+"</a>"

    val a = new Article(
      title = if (title.nonEmpty) title else albumDir.getName,
      slug = generateSlug(albumDir.getName),
      date = date,
      rawText = mkText(imgs),
      images = imgs
    )

    (a, a.copy(rawText = mkSample(a)))
  }).unzip
}



val (articles, indexArticles, tagMap): (Seq[Article], Seq[Article], Map[String, Seq[Article]]) =
  Blog.kind match {
    case "blog" =>
      val (as, tm) = prepareBlog()
      (as, as, tm)

    case "gallery" =>
      val (as, is) = prepareGallery()
      (as, is, Map())
    case _ => sys.error("wut")
  }



val fileIndex = mutable.ArrayBuffer[(String, String)]()

// make index
val isIndexGallery = articles.take(Blog.articlesOnIndex).exists(_.images.nonEmpty)

val path = "index.html"
val url  =  absUrlFromPath(path)
val l = new FlowLayout(url)
val content = l.makeIndex(indexArticles)
fileIndex += saveFile(path, l.makePage(content, gallery = isIndexGallery))

// make articles
articles foreach { a =>
  val path = relUrlFromSlug(a.slug)
  val url  = absUrlFromSlug(a.slug)
  var l = new FlowLayout(url)
  val content = l.makeFullArticle(a, articles, true, true)
  fileIndex += saveFile(path, l.makePage(content, title = a.title, gallery = a.images.nonEmpty))
}

// make tag pages
tagMap foreach { case (t, as) =>
  val path = relUrlFromSlug(tagSlug(t))
  val url  = absUrlFromSlug(tagSlug(t))
  var l = new FlowLayout(url)
  val content = l.makeTagPage(t, as)
  fileIndex += saveFile(path, l.makePage(content, title = t, rss = tagSlug(t)+".xml"))
  fileIndex += saveXml(tagSlug(t), generateRSS(as))
}

// make RSS
fileIndex += saveXml("rss", generateRSS(articles))


// make thumbnails
val images = articles.flatMap(_.images)
new File("t").mkdir()
for (image <- images) {
  val (w, h) = (Blog.thumbWidth, Blog.thumbHeight)
  val thumbFile = new File(thumbnailUrl(image))
  if (!thumbFile.exists) {
    println(s"resizing image ${image.url} -> $thumbFile")
    try {
      val full = ImageIO.read(new URL(image.url.replaceAll(" ", "%20"))) // TODO hackity hack
      val resized = resizeImage(full, w, h)
      ImageIO.write(resized, "jpg", thumbFile)
    } catch { case e: javax.imageio.IIOException =>
      println(e)
    }
  }
}

// make robots.txt
fileIndex += saveFile("robots.txt", "User-agent: *\nAllow: /")

//fileIndex += saveFile("sitemap.xml", "<?xml version=\"1.0\" encoding=\"utf-8\"?>" +
//  <urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
//  { articles map { a => <url><loc>{absUrlFromSlug(a.slug)}</loc></url> } }
//  </urlset>
//)

// make file index
saveFile(".files", fileIndex.map { case (file, hash) => file+" "+hash }.mkString("\n"))
