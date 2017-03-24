import java.util.{ Date, GregorianCalendar, Locale }
import java.text.SimpleDateFormat


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
}

class Article(
  val title: String,
  val slug: String,
  val date: Date,
  val tags: Seq[String],
  val text: String
) {
  override def toString = "Article("+title+")"
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
val linkRefRegex = """^\[(.*?)\]: (.+)$""".r
val dateRegex    = """^(\d+)-(\d+)-(\d+)$""".r
val tagsRegex    = """^#\[(.+)\]$""".r

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

def parseArticle(ls: Vector[String]): Article = {
  val lines = ls.map(_.trim)

  val titleLine = lines(0)
  val titleRegex(title, slug) = titleLine

  val (meta, _body) = lines.drop(2).span(l => l.nonEmpty)
  val body = _body.dropWhile(l => l.isEmpty).reverse.dropWhile(l => l.isEmpty).reverse

  val (date, _m1) = process(meta, parseDate)
  val (tags, _m2) = process(_m1, parseTags)

  if (_m2.nonEmpty) sys.error("some metainformation was not processed: "+_m2)

  val linkRefs = body.filter { l => l.matches("^\\[.*\\]: .*$") }
  val txtLines = body.map {
    case l if l.matches("^\\[\\w+\\]: .*$") => "<span class=y>"+l+"</span>"
    case l => l
  }

  val linkMap = linkRefs.map { l =>
    val linkRefRegex(r, url) = l
    (r, url)
  }.toMap

  new Article(
    title = title.trim,
    slug = if (slug == null || slug == "") generateSlug(title) else slug,
    date = date.getOrElse(null),
    tags = tags.getOrElse(Seq()),
    text = decorateText(txtLines.mkString("\n"), linkMap)
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

def decorateText(text: String, linkMap: Map[String, String]) = {
  var txt = text

  txt = """(?x) " ([^"]+(?:\R[^"]+)?) " : \[ (\w+) \]""".r.replaceAllIn(txt, m => {
    val url = relativizeUrl(linkMap(m.group(2)))
    s"""<span class=l>"<a href="${url}">${m.group(1)}</a>":[${m.group(2)}]</span>"""
  })

  txt = """(?xm) ( (?: ^>[^\n]*\n)+ )""".r.replaceAllIn(txt, m =>
    "<blockquote>"+m.group(1).replaceAll(">", "&gt;")+"</blockquote>"
  )

  txt
    .replaceAll("""(?xs)\*\*(.+?)\*\*""",
      """<b>**<span>$1</span>**</b>""")
    .replaceAll("""(?xs)(?<!\*)\*(?!\*)(.+?)(?<!\*)\*(?!\*)""",
      """<i>*<span>$1</span>*</i>""")
//    .replaceAll("""(?s)_(.+?)_""",
//      """<u><span class=y>_</span>$1<span class=y>_</span></u>""")
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
