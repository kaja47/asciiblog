import java.util.{ Date, GregorianCalendar, Locale }
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
  val pageWidth: Int       = cfg.getOrElse("pageWidth", "80").toInt
  val style: String        = cfg.getOrElse("style", "")
  val thumbWidth: Int      = cfg.getOrElse("thumbnailWidth", "150").toInt
  val thumbHeight: Int     = cfg.getOrElse("thumbnailHeight", "100").toInt
  val limitRss: Int        = cfg.getOrElse("limitRss", Int.MaxValue.toString).toInt
}

case class Article(
  title: String,
  slug: String,
  date: Date,
  tags: Tags = Tags(),
  rawText: String,
  images: Seq[Image] = Seq(),
  links: Seq[String] = Seq(),
  linkMap: Map[String, String] = Map(),
  backlinks: Seq[Article] = Seq(),
  similar: Seq[Article] = Seq()
) {
  override def toString = {
    val f = new SimpleDateFormat("MM-dd-yyyy")
    "Article("+(if (date == null) "" else f.format(date)+" ")+title+")"
  }
}

case class Tags(visible: Seq[String] = Seq(), hidden: Seq[String] = Seq())
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



//def url(slug: String) = Blog.baseUrl + "/" + slug + ".html"
def absUrl(slug: String) = Blog.baseUrl + "/" + slug + ".html"
def relUrl(slug: String) = slug + ".html"
def relativizeUrl(url: String) = if (localLink(url)) dropLocalPrefix(url) else url
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


val titleRegex   = """^(.+?)(?:\[([^ ]+)\])?$""".r
val linkRefBlockRegex = """(?xm) (?:  ^\[(.*?)\]:\ (.+)\n  )*  ^\[(.*?)\]:\ (.+) """.r
val linkRefRegex      = """(?xm)      ^\[(.*?)\]:\ (.+)$""".r
val dateRegex    = """^(\d+)-(\d+)-(\d+)$""".r
val tagsRegex    = """^#\[(.+)\]$""".r

val boldRegex     = """(?xs)\*\*(.+?)\*\*""".r
val italicRegex   = """(?xs)(?<!\*)\*(?!\*)(.+?)(?<!\*)\*(?!\*)""".r
val linkRegex     = """(?x) " ([^"]+(?:\R[^"]+)?) " : \[ (\w+) \]""".r
val imgBlockRegex = """(?xm) (?: ^\[\*\ + [^* ]+ \ +\*\]\ *\n)+   (?: ^\[\*\ + [^* ]+ \ +\*\]\ *\n?) """.r
val imgRegex      = """(?xm)      \[\*\ +([^* ]+)\ +\*\]\ * """.r
val blockquoteRegex = """(?xm) ( (?: ^>[^\n]*\n)+ )""".r
val blackoutRegex = """(?xs) \[\|.+?\|\] """.r

def parseDate(l: String): Option[Date] = l match {
  case dateRegex(y, m, d) => Some(new GregorianCalendar(y.toInt, m.toInt-1, d.toInt).getTime)
  case _ => None
}

def parseTags(l: String): Option[Tags] = l match {
  case tagsRegex(ts) =>
    val (hidden, visible) = (ts.split(",").map(_.trim).partition(t => t.startsWith("(") && t.endsWith(")")))
    Some(Tags(visible, hidden.map(_.drop(1).dropRight(1))))
  case _ => None
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

  val (meta, _body) = ls.drop(2).span(l => l.nonEmpty)
  val body = _body.dropWhile(l => l.isEmpty).reverse.dropWhile(l => l.isEmpty).reverse

  val (date, _m1) = process(meta, parseDate)
  val (tags, _m2) = process(_m1, parseTags)

  if (_m2.nonEmpty) sys.error("some metainformation was not processed: "+_m2)

  val linkMap = body.collect {
    case linkRefRegex(r, url) => (r, url)
  }.toMap

  val images = body.collect { case imgRegex(url) => new Image(url, hash(url)) }

  new Article(
    title   = title.trim,
    slug    = if (slug == null || slug == "") generateSlug(title) else slug,
    date    = date.getOrElse(null),
    tags    = tags.getOrElse(Tags()),
    rawText = body.mkString("\n"),
    images  = images,
    linkMap = linkMap,
    links   = linkMap.values.toVector
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

def nextPrev(a: Article, as: Seq[Article]): (Article, Article) = {
  val pos = as.indexOf(a)
  assert(pos != -1)
  (if (a == as.head) null else as(pos-1), if (a == as.last) null else as(pos+1))
}

def similarByTags(a: Article, tagMap: Map[String, Seq[Article]]): Seq[Sim] = {
  def dateDiff(a: Article, b: Article): Long = math.abs(a.date.getTime - b.date.getTime)

  (a.tags.visible ++ a.tags.hidden)
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
  def makePage(content: String, title: String, gallery: Boolean): String
  def makeIndex(articles: Seq[Article]): String
  def makeFullArticle(a: Article, as: Seq[Article], prevNextNavigation: Boolean, tags: Boolean): String
  def makeTagPage(t: String, as: Seq[Article]): String
}

trait FlowLayout extends Layout {
  def decorateText(a: Article): String = {
    var txt = a.rawText
    txt = blackout(txt)
    txt = linkRegex.replaceAllIn(txt, m => {
      val ref = m.group(2)
      val url = relativizeUrl(a.linkMap.getOrElse(ref, throw new Exception(s"link reference [$ref] is not defined in article '${a.title}'")))
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

  def makePage(content: String, title: String, gallery: Boolean): String = {
s"""<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8" />
<title>${(if (title != null) title+" | " else "")+Blog.title}</title>
<link rel="alternate" type="application/rss+xml" href="rss.xml"/>
<style>a{color:inherit}blockquote{margin:0;padding:0;font-style:italic;}.r{text-align:right}.f{float:right}.b{max-width:46em;font-family:monospace}.th,.thz{width:${Blog.thumbWidth}px;height:${Blog.thumbHeight}px} ${Blog.style}</style>
${if (gallery) { "<script>"+galleryScript+"</script>" } else ""}
</head>
<body>
<div class=b>
<div class=r><b><a href="index.html">${Blog.title}</a></b> [<a href="rss.xml">RSS</a>]</div>
"""+content+"""
</div>
</body>
</html>"""
  }

  def makeIndex(articles: Seq[Article]): String = {
    val (fulls, links) = articles.splitAt(Blog.articlesOnIndex)
    (fulls.map(a => makeFullArticle(a, articles, false, false)) ++ links.map(makeLink)).mkString("<br/>") + "<br/>"
  }
  def makeFullArticle(a: Article, as: Seq[Article], prevNextNavigation: Boolean, tags: Boolean): String = {
    val sims = a.similar.take(5)

    makeLink(a)+
    (if (prevNextNavigation) "<span class=f>"+makeNextPrevArrows(a, as)+"</span>" else "")+
    "<br/><br/><br/>\n"+
    decorateText(a)+
    (if (prevNextNavigation) makeNextPrevLinks(a, as) else "")+{
      val rel =
        (if (tags && a.tags.visible.nonEmpty) makeTagLinks(a.tags.visible)+"<br/>" else "")+
        (if (tags && (a.backlinks.nonEmpty || sims.nonEmpty)) makeRelLinks(Seq(sims, a.backlinks))+"<br/>" else "")

      if (rel.nonEmpty) "<div class=r>"+rel+"</div>" else ""
    }
  }

  def makeTagPage(t: String, as: Seq[Article]) = {
    makeTagLink(t)+"<br/><br/>"+
    as.map(makeLink).mkString("<br/>")
  }

  def blackout(txt: String) =
    blackoutRegex.replaceAllIn(txt, m => m.group(0).replaceAll("(?s).", "█").grouped(5).mkString("<wbr>"))

  def makeLink(a: Article) =
    makeDate(a)+s"""<i><a href="${relUrl(a.slug)}">${a.title}</a></i>"""

  def makeTagLink(t: String) =
    s"""<span class=y>#</span><i><a href="${relUrl(tagSlug(t))}">${t}</a></i>"""

  def makeRelLink(a: Article, ord: Int) =
    s"""<i><a href="${relUrl(a.slug)}">#${ord}</a></i>"""

  def makeNextPrevLinks(a: Article, as: Seq[Article]) = {
    val (prev, next) = nextPrev(a, as)
    (if (prev == null) "" else "&lt;&lt;&lt; "+makeLink(prev)+"<br/>") +
    (if (next == null) "" else "&gt;&gt;&gt; "+makeLink(next)+"<br/>")
  }

  def makeNextPrevArrows(a: Article, as: Seq[Article]) = {
    val (prev, next) = nextPrev(a, as)
    (if (prev == null) "&nbsp;&nbsp;&nbsp;" else s"""<a id=prev href="${relUrl(prev.slug)}">&lt;&lt;&lt;</a>""")+" "+
    (if (next == null) "&nbsp;&nbsp;&nbsp;" else s"""<a id=next href="${relUrl(next.slug)}">&gt;&gt;&gt;</a>""")
  }

  def makeTagLinks(ts: Seq[String]) =
    ts.map(makeTagLink).mkString(" ")

  def makeRelLinks(ass: Seq[Seq[Article]]) =
    ass.map { as =>
      as.zipWithIndex.map { case (a, i) => makeRelLink(a, i+1) }.mkString(" ")
    }.mkString(" ")

  def makeDate(a: Article) =
    if (a.date == null) ""
    else new SimpleDateFormat("d. M.").format(a.date)+" "
}




def rssdate(date: Date) = if (date == null) "" else
  new SimpleDateFormat("EEE', 'dd' 'MMM' 'yyyy' 'HH:mm:ss' 'Z", Locale.US).format(date)

def generateRSS(articles: Seq[Article]): String = {
  "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n" + (
<rss version="2.0">
<channel>
<title>{Blog.title}</title>
{articles.take(Blog.limitRss).map { a =>
<item><title>{a.title}</title><guid isPermaLink="true">{absUrl(a.slug)}</guid><pubDate>{rssdate(a.date)}</pubDate></item>
}}
</channel>
</rss>).toString
}



def saveHtml(f: String, content: String): (String, String) =
  saveFile(f+".html", content)

def saveFile(f: String, content: String): (String, String) = {
  val fw = new java.io.FileWriter(f)
  fw.write(content)
  fw.close()
  (f, hash(content))
}



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

  val (hidden, rest1) = articles.span { a => a.title.startsWith("?") }
  val (visible, rest) = rest1.span { a => !a.title.startsWith("?") }
  if (rest.nonEmpty) sys.error("hidden and visible articles are mixed up")

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
  val ordered = articles
    .filter(_.date != null)
    .map(_.date)
    .sliding(2)
    .forall { case Seq(a, b) => a.compareTo(b) >= 0 }

  if (!ordered) sys.error("articles are not ordered by date")

  val tagMap: Map[String, Seq[Article]] =
    articles
      .flatMap { a => a.tags.visible.map { t => (t, a) } }
      .groupBy(_._1)
      .map { case (t, tas) => (t, tas.map { _._2 }) }

  val allTagMap: Map[String, Seq[Article]] =
    articles
      .flatMap { a => (a.tags.visible ++ a.tags.hidden).map { t => (t, a) } }
      .groupBy(_._1)
      .map { case (t, tas) => (t, tas.map { _._2 }) }


  val backlinks: Map[String, Seq[Article]] =
    (for {
      a <- articles
      l <- a.links
      if localLink(l)
    } yield (extractSlug(l), a))
      .groupBy(_._1)
      .map { case (slug, as) => (slug, as.map { _._2 }) }

  val as = articles map { a =>
    val bs = backlinks.getOrElse(a.slug, Seq())

    a.copy(
      backlinks = bs,
      similar = similarByTags(a, allTagMap, without = bs)
    )
  }

  (as, tagMap)
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
      s"""<a href="${relUrl(a.slug)}">"""+
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



val layout = new FlowLayout {}
import layout._

val fileIndex = mutable.ArrayBuffer[(String, String)]()

// make index
val isIndexGallery = articles.take(Blog.articlesOnIndex).exists(_.images.nonEmpty)
fileIndex += saveFile("index.html", makePage(makeIndex(indexArticles), null, isIndexGallery))

// make articles
articles foreach { a =>
  fileIndex += saveHtml(a.slug, makePage(makeFullArticle(a, articles, true, true), a.title, a.images.nonEmpty))
}

// make tag pages
tagMap foreach { case (t, as) =>
  fileIndex += saveHtml(tagSlug(t), makePage(makeTagPage(t, as), null, false))
}

// make RSS
fileIndex += saveFile("rss.xml", generateRSS(articles))


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

fileIndex += saveFile("sitemap.xml", "<?xml version=\"1.0\" encoding=\"utf-8\"?>" +
  <urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
  { articles map { a => <url><loc>{absUrl(a.slug)}</loc></url> } }
  </urlset>
)

// make file index
saveFile(".files", fileIndex.map { case (file, hash) => file+" "+hash }.mkString("\n"))
