import java.util.{ Date, GregorianCalendar, Locale }
import java.text.SimpleDateFormat
import java.io.File
import java.net.URL
import java.awt.image.{ BufferedImage, ConvolveOp, Kernel }
import javax.imageio.ImageIO


val cfg = io.Source.fromFile(args(0))
  .getLines
  .map(l => l.split(" ", 2))
  .map { case Array(k, v) => (k, v)}
  .toMap

object Blog {
  val title: String   = cfg("title")
  val baseUrl: String = cfg("baseUrl")
  val fullArticlesOnIndex: Int = cfg("fullArticlesOnIndex").toInt
  val pageWidth: Int =  cfg("pageWidth").toInt
  val style: String = cfg("style")
  val thumbWidth: Int = 150
  val thumbHeight: Int = 100
}

class Article(
  val title: String,
  val slug: String,
  val date: Date,
  val tags: Seq[String],
  val text: String,
  val images: Seq[Image]
) {
  override def toString = "Article("+title+")"
}

case class Image(
  val url: String,
  val thumb: String
)


def resizeImage(src: BufferedImage, width: Int, height: Int) = {
  val blur = new ConvolveOp(
    new Kernel(5, 5, Array.fill[Float](25)(1f/25)),
    ConvolveOp.EDGE_NO_OP, null
  )

  val zoom = math.min(1.0 * src.getWidth / width, 1.0 * src.getHeight / height)
  val wz = (width * zoom).toInt
  val hz = (height * zoom).toInt
  val x = (src.getWidth - wz) / 2
  val y = (src.getHeight - hz) / 2
  val crop = blur.filter(src.getSubimage(x, y, wz, hz), null)

  val tpe = if (crop.getType == BufferedImage.TYPE_CUSTOM) BufferedImage.TYPE_INT_ARGB else crop.getType
  val thumb = new BufferedImage(width, height, tpe)
  val g = thumb.createGraphics()
  g.drawImage(crop, 0, 0, width, height, null)
  g.dispose()

  thumb
}


//def url(slug: String) = Blog.baseUrl + "/" + slug + ".html"
def absUrl(slug: String) = Blog.baseUrl + "/" + slug + ".html"
def relUrl(slug: String) = slug + ".html"
def relativizeUrl(url: String) =
  if (url.startsWith(Blog.baseUrl+"/")) url.drop(Blog.baseUrl.length+1)
  else url


def getArticle(lines: Vector[String]): (Article, Vector[String]) = {

  val underlinePos = lines.indexWhere(l => l.startsWith("==="), 2)

  if (underlinePos == -1) {
    (parseArticle(lines), Vector())
  } else {
    val (art, rest) = lines.splitAt(underlinePos-1)
    (parseArticle(art), rest)
  }
}


val titleRegex   = """^(.*?)(?:\[(.*)\])?$""".r
val linkRefRegex = """(?xm) ^\[(.*?)\]:\ (.+)$""".r
val dateRegex    = """^(\d+)-(\d+)-(\d+)$""".r
val tagsRegex    = """^#\[(.+)\]$""".r

val imgBlockRegex = """(?xm) (?: ^\[\*\ +(\S+)\ +\*\]\ *\n)+ """.r
val imgRegex      = """(?xm) \[\*\ +(\S+)\ +\*\]\ * """.r

def parseDate(l: String): Option[Date] = l match {
  case dateRegex(y, m, d) => Some(new GregorianCalendar(y.toInt, m.toInt-1, d.toInt).getTime)
  case _ => None
}

def parseTags(l: String): Option[Seq[String]] = l match {
  case tagsRegex(ts) => Some(ts.split(",").map(_.trim))
  case _ => None
}

def process[T](ls: Seq[String], f: String => Option[T]): (Option[T], Seq[String]) = {
  val pairs = ls.zip(ls.map(f))
  val idx = pairs.indexWhere { case (l, v) => v != None }
  if (idx == -1) (None, ls) else {
    (pairs(idx)._2, ls.patch(idx, Seq(), 1))
  }
}

def parseArticle(lines: Vector[String]): Article = {
  val ls = lines.map(_.trim)

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

  def hash(url: String) = {
    val md5 = java.security.MessageDigest.getInstance("MD5")
    md5.reset()
    val digest = md5.digest(url.getBytes("utf-8"))
    val bigInt = new java.math.BigInteger(1, digest)
    bigInt.toString(16).reverse.padTo(32, '0').reverse
  }

  val images = body.collect { case imgRegex(url) => new Image(url, hash(url)) }

  new Article(
    title = title.trim,
    slug = if (slug == null || slug == "") generateSlug(title) else slug,
    date = date.getOrElse(null),
    tags = tags.getOrElse(Seq()),
    text = decorateText(body.mkString("\n"), linkMap, images),
    images = images
  )
}

def generateSlug(title: String) = {
  val from = "áčďéěíňóřšťúůýž"
  val to   = "acdeeinorstuuyz"
  val txl = (from zip to).toMap withDefault (x => x)

  title
    .toLowerCase
    .map(txl)
    .map{ ch => if (!Character.isAlphabetic(ch)) "-" else ch }
    .mkString
    .replaceAll("--+", "-")
    .replaceAll("-+$", "")
    .replaceAll("^-+", "")
}

def tagSlug(title: String) = "tag-"+generateSlug(title)

def decorateText(text: String, linkMap: Map[String, String], images: Seq[Image]) = {
  var txt = text

  txt = """(?x) " ([^"]+(?:\R[^"]+)?) " : \[ (\w+) \]""".r.replaceAllIn(txt, m => {
    val url = relativizeUrl(linkMap(m.group(2)))
    s"""<span class=l>"<a href="${url}">${m.group(1)}</a>":[${m.group(2)}]</span>"""
  })

  txt = imgBlockRegex.replaceAllIn(txt, m => {
    val links = imgRegex.findAllMatchIn(m.group(0)).map(_.group(1)).toVector
    val block = links.map { l =>
      val thumbPath = "t/"+{images.find(i => i.url == l).get.thumb}
      s"""<a href="$l"><img src="$thumbPath"/></a>"""
    }.mkString("")
    block
  })

  txt = """(?xm) ( (?: ^>[^\n]*\n)+ )""".r.replaceAllIn(txt, m =>
    "<blockquote>"+m.group(1).replaceAll(">", "&gt;")+"</blockquote>"
  )

  txt = linkRefRegex.replaceAllIn(txt, m => "<span class=y>"+m.group(0)+"</span>")

  txt
    .replaceAll("""(?xs)\*\*(.+?)\*\*""",
      """<b>**<span>$1</span>**</b>""")
    .replaceAll("""(?xs)(?<!\*)\*(?!\*)(.+?)(?<!\*)\*(?!\*)""",
      """<i>*<span>$1</span>*</i>""")
//    .replaceAll("""(?s)_(.+?)_""",
//      """<u><span class=y>_</span>$1<span class=y>_</span></u>""")
  txt
}

def makePage(content: String) = {
s"""<meta charset="utf-8" />
<title>${Blog.title}</title>
<link rel="alternate" type="application/rss+xml" href="rss.xml" />
<style>a{color:inherit} i, b, .y{color:#999} i span, b span, i a, .l a{color:black} .l{color:#bbb} blockquote {margin:0;paddig:0;font-style:italic;} ${Blog.style}</style>

<pre>
${alignSpace(Blog.title)}<a href="index.html">${Blog.title}</a> [<a href="rss.xml">RSS</a>]
"""+content+"\n</pre>"
}

def alignSpace(str: String, skip: Int = 0) = " "*(Blog.pageWidth-str.length-skip)



def rssdate(date: Date) = if (date == null) "" else
  new SimpleDateFormat("EEE', 'dd' 'MMM' 'yyyy' 'HH:mm:ss' 'Z", Locale.US).format(date)

def generateRSS(articles: Seq[Article]): String = {
  "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n" + (
<rss version="2.0">
<channel>
  <title>blog feed</title>
  {articles.map { a =>
  <item>
    <title>{a.title}</title>
    <guid isPermaLink="true">{absUrl(a.slug)}</guid>
    <pubDate>{rssdate(a.date)}</pubDate>
  </item>
  }}
</channel>
</rss>).toString
}



def saveHtml(f: String, content: String): Unit = {
  saveFile(f+".html", content)
}

def saveFile(f: String, content: String): Unit = {
  val fw = new java.io.FileWriter(f)
  fw.write(content)
  fw.close()
}


var lines = io.Source.fromFile(args(1)).getLines.toVector
var articles = List[Article]()
val today = new Date

while (lines.nonEmpty) {
  val (a, ls) = getArticle(lines)

  val isInPast = a.date == null || a.date.before(today)
  if (!a.title.startsWith("?") && isInPast) {
    articles ::= a
  }
  lines = ls

}

articles = articles.reverse

val tagMap = 
  articles
    .flatMap { a => a.tags.map { t => (t, a) } }
    .groupBy(_._1)
    .map { case (t, tas) => (t, tas.map { _._2 }) }


def makeLink(a: Article) =
  makeDate(a)+s"""<i><a href="${relUrl(a.slug)}">${a.title}</a></i>"""

def makeTagLink(t: String) =
  s"""<span class=y>#</span><i><a href="${relUrl(tagSlug(t))}">${t}</a></i>"""

def makeDate(a: Article) =
  if (a.date == null) ""
  else new SimpleDateFormat("d. M.").format(a.date)+" "

def makeFullArticle(a: Article, as: List[Article], prevNextNavigation: Boolean, tags: Boolean) = {
  val titleLength = makeDate(a).length + a.title.length

  makeLink(a)+
  (if (prevNextNavigation) alignSpace("<<< >>>", titleLength)+makeNextPrevArrows(a, as) else "")+
  "\n"+
  //"="*a.title.length+"\n"+
  "\n\n"+
  a.text+"\n\n\n"+
  (if (prevNextNavigation) makeNextPrevLinks(a, as) else "")+
  (if (tags) makeTagLinks(a.tags) else "")

}

def makeNextPrevLinks(a: Article, as: List[Article]) = {
  val pos = as.indexOf(a)
  assert(pos != -1)

  (if (a == as.head) "" else "&lt;&lt;&lt; "+makeLink(as(pos-1))+"\n") +
  (if (a == as.last) "" else "&gt;&gt;&gt; "+makeLink(as(pos+1))+"\n")
}

def makeNextPrevArrows(a: Article, as: List[Article]) = {
  val pos = as.indexOf(a)
  assert(pos != -1)

  (if (a == as.head) "   " else s"""<a href="${relUrl(as(pos-1).slug)}">&lt;&lt;&lt;</a>""")+" "+
  (if (a == as.last) "   " else s"""<a href="${relUrl(as(pos+1).slug)}">&gt;&gt;&gt;</a>""")
}

def makeTagLinks(ts: Seq[String]) =  {
  val len = ts.map(t => t.length + 2).sum - 1
  alignSpace("", len)+ts.map(makeTagLink).mkString(" ")
}
  

def makeTagPage(t: String, as: Seq[Article]) = {
  makeTagLink(t)+"\n\n"+
  as.map(makeLink).mkString("\n")
}


// slug duplicates
articles.groupBy(_.slug) foreach { case (slug, as) =>
  if (as.size > 1) {
    println("multiple articles with the same slug '"+slug+"'")
    sys.exit()
  }
}

// make index
val indexContent =  {
  val (fulls, links) = articles.splitAt(Blog.fullArticlesOnIndex)
  (fulls.map(a => makeFullArticle(a, articles, false, false)) ++ links.map(makeLink)).mkString("\n")
}

saveFile("index.html", makePage(indexContent))

// make articles
articles foreach { a =>
  saveHtml(a.slug, makePage(makeFullArticle(a, articles, true, true)))
}

// make tag pages
tagMap foreach { case (t, as) =>
  saveHtml(tagSlug(t), makePage(makeTagPage(t, as)))
}

// make RSS
saveFile("rss.xml", generateRSS(articles))


// make thumbnails
val images = articles.flatMap(_.images)
new File("t").mkdir()
for (image <- images) {
  val thumbFile = new File("t/"+image.thumb)
  if (!thumbFile.exists) {
    val full = ImageIO.read(new URL(image.url))
    val resized = resizeImage(full, Blog.thumbWidth, Blog.thumbHeight)
    ImageIO.write(resized, "jpg", thumbFile)
  }
}
