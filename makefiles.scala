import java.util.{ Date, GregorianCalendar, Calendar, Locale }
import java.text.SimpleDateFormat
import java.io.File
import java.net.{ URL, URI }
import java.awt.image.{ BufferedImage, ConvolveOp, Kernel }
import java.awt.{ AlphaComposite, RenderingHints => RH }
import javax.imageio.ImageIO
import scala.collection.mutable
import scala.util.matching.Regex


val cfg = io.Source.fromFile(args(0))
  .getLines
  .map(kv)
  .toMap

def kv(l: String) = { val Array(k, v) = l.split(" ", 2) ; (k, v) }

def timer[T](label: String)(f: => T) = {
  val s = System.currentTimeMillis
  val r = f
  val e = System.currentTimeMillis
  println(s"$label ${e-s} ms")
  r
}

val thisDir =
  new File(System.getProperty("sun.java.command").split(" ").find(_.endsWith("makefiles.scala")).get).getParent

val galleryScript =
  io.Source.fromFile(thisDir+"/gallery.js").mkString
    .replaceAll("(?<!let|function|in)[\\s]+(?!in)|/\\*.*?\\*/", "") // rather crude and incorrect minifier

object Blog {
  val kind: String         = cfg.getOrElse("type", "blog")
  val title: String        = cfg("title")
  val baseUrl: String      = cfg("baseUrl")
  val files: Seq[String]   = cfg.getOrElse("files", "").split(" ").filter(_.nonEmpty)
  val articlesOnIndex: Int = cfg.getOrElse("fullArticlesOnIndex", "5").toInt
  val style: String        = cfg.getOrElse("style", "")
  val thumbWidth: Int      = cfg.getOrElse("thumbnailWidth", "150").toInt
  val thumbHeight: Int     = cfg.getOrElse("thumbnailHeight", "100").toInt
  val limitRss: Int        = cfg.getOrElse("limitRss", Int.MaxValue.toString).toInt
  val sortByDate: Boolean  = cfg.getOrElse("sortByDate", "false").toBoolean
  val imageRoot: String    = cfg.getOrElse("imageRoot", "")
  val articlesMustBeSorted: Boolean = cfg.getOrElse("articlesMustBeSorted", "true").toBoolean
  val articlesMustNotBeMixed: Boolean = cfg.getOrElse("articlesMustNotBeMixed", "true").toBoolean
  val translation: Map[String, String] = io.Source.fromFile(thisDir+"/lang.cs").getLines.map(kv).toMap
}

case class Base(all: Seq[Article], tagMap: Map[String, Seq[Article]] = Map()) {
  lazy val bySlug: Map[String, Article] = all.map(a => (a.slug, a)).toMap
  lazy val byMeta: Map[String, Article] = all.flatMap(a => a.meta.scalars collect { case m: String => (m, a) }).toMap
  lazy val feed = all.filter(_.inFeed)
  private lazy val art2ord = feed.zipWithIndex.toMap

  def find(id: String): Option[Article] = bySlug.get(id).orElse(byMeta.get(id))
  def isValidId(id: String) = find(id).nonEmpty

  def next(a: Article) = art2ord.get(a).flatMap { ord => feed.lift(ord+1) }.getOrElse(null)
  def prev(a: Article) = art2ord.get(a).flatMap { ord => feed.lift(ord-1) }.getOrElse(null)

  private def move(a: Article, tag: String, n: Int) = {
    val as = tagMap(tag)
    val pos = as.indexWhere(_.slug == a.slug)
    require(pos != -1)
    if (pos+n < 0 || pos+n >= as.length) null
    else as(pos+n)
  }

  def next(a: Article, tag: String) = move(a, tag, +1)
  def prev(a: Article, tag: String) = move(a, tag, -1)
}

class Similarities(tagMap: Map[String, Seq[Article]]) {
  private val arts: Array[Article] = tagMap.values.flatten.toArray.distinct
  private val artMap: Map[Article, Int] = arts.zipWithIndex.toMap
  private val tm: Map[String, Array[Int]] = tagMap.map { case (t, as) => (t, as.map(artMap).toArray) }

  // TODO use a.meta.seq("rel")
  def similarByTags(article: Article): Seq[Sim] = {
    def dateDiff(a: Article, b: Article): Long = {
      (a.date, b.date) match {
        case (null, null) => 0
        case (null, _) | (_, null) => Long.MaxValue/2
        case (a, b) => math.abs(a.getTime - b.getTime)
      }
    }
    val freq = new Array[Int](arts.length) // article idx -> count
    val alltags = article.tags.supertags ++ article.tags.visible ++ article.tags.hidden
    for (t <- alltags) {
      tm.get(t) foreach { arr =>
        var i = 0; while (i < arr.length) {
          freq(arr(i)) += 1
          i += 1
        }
      }
    }

    (0 until arts.length).iterator.collect { case i if freq(i) >= 1 && arts(i).slug != article.slug =>
      Sim(arts(i), freq(i))
    } .toVector
      .sortBy { s => (~s.commonTags, dateDiff(article, s.article)) } // most common tags, published closest together
  }

  def similarByTags(a: Article, count: Int, without: Seq[Article]): Seq[Article] =
    similarByTags(a).filter(s => !without.contains(s.article)).take(count).map(_.article)
}

case class Article(
  title: String,
  slug: String,
  date: Date,
  tags: Tags = Tags(),
  license: String = null,
  meta: Meta = Meta(),
  rawText: String,
  images: Seq[Image] = Seq(),
  linkMap: Map[String, String] = Map(), // map of link refs only
  links: Seq[String] = Seq(), // all link bodies (inside "":[ref] or href="url")
  backlinks: Seq[Article] = Seq(),
  similar: Seq[Article] = Seq(),
  pub: Seq[Article] = Seq(),
  pubBy: Article = null,
  inFeed: Boolean = true
) {
  def prettyDate = if (date == null) "" else new SimpleDateFormat("MM-dd-yyyy").format(date)
  override def toString = s"Article($prettyDate $title)"
  def asSlug: Slug = Slug(slug)
  def format = meta.scalar("format")
}

case class Meta(xs: Map[String, Meta] = Map()) {
  def scalar(key: String): String = scalars.find(_.startsWith(key+":")).map(_.drop(key.length+1).trim).getOrElse(null)
  def scalars = xs.collect { case (k, v) if v == null => k }
  def seq(k: String): Seq[String] = xs.get(k).map(_.xs.keys.toSeq).getOrElse(Seq())
}

case class Image(
  url: String,
  thumb: String,
  alt: String = null,
  mods: String = null,
  align: String = null,
  title: String = null,
  license: String = null,
  source: String = null
)

case class Slug(id: String)
case class Tags(visible: Seq[String] = Seq(), hidden: Seq[String] = Seq(), supertags: Seq[String] = Seq())
case class Sim(article: Article, commonTags: Int)

def resizeImage(src: BufferedImage, width: Int, height: Int): BufferedImage = {
  val zoom = math.min(1.0 * src.getWidth / width, 1.0 * src.getHeight / height)
  val wz = math.max(1, (width * zoom).toInt)
  val hz = math.max(1, (height * zoom).toInt)
  val x = (src.getWidth - wz) / 2
  val y = (src.getHeight - hz) / 2
  val crop = src.getSubimage(x, y, wz, hz)
  progressiveResize(crop, width, height)
}

/** adapted from https://github.com/coobird/thumbnailator/blob/master/src/main/java/net/coobird/thumbnailator/resizers/ProgressiveBilinearResizer.java */
def progressiveResize(src: BufferedImage, width: Int, height: Int): BufferedImage = {
  val tpe = if (src.getType == BufferedImage.TYPE_CUSTOM) BufferedImage.TYPE_INT_ARGB else src.getType
  val dest = new BufferedImage(width, height, tpe)

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
def absUrl(a: Article) = absUrlFromSlug(a.slug)
def relUrlFromSlug(slug: String) = slug + ".html"
def fixPath(path: String) = path.replaceAll(" ", "%20") // TODO hackity hack
def isAbsolute(url: String) = new URI(fixPath(url)).isAbsolute
def relativize(url: String, baseUrl: String) = {
  val u = new URI(url)
  val b = new URI(baseUrl)
  require(b.isAbsolute)

  if (u.getHost != null && u.getHost != b.getHost) {
    url

  } else if (u.getPath == null) { // mailto: links
    url

  } else {
    val us = u.getPath.split("/").filter(_.nonEmpty)
    val bs = b.getPath.split("/").filter(_.nonEmpty)

    val prefixLen = (us zip bs).takeWhile { case (u, b) => u == b }.length

    val backLevels = bs.length - prefixLen - 1
    (Seq.fill(backLevels)("..") ++ us.drop(prefixLen)).mkString("/")
  }
}
def isLocalLink(url: String) = url.startsWith(Blog.baseUrl+"/")
def dropLocalPrefix(url: String) = url.drop(Blog.baseUrl.length+1)
def extractSlug(url: String) = if (isLocalLink(url)) Slug(dropLocalPrefix(url).dropRight(5)) else sys.error("not local url")
def thumbnailUrl(img: Image) = s"t/${img.thumb}-${Blog.thumbWidth}x${Blog.thumbHeight}"

def undefRefError(ref: String, a: Article) =
  println(s"link reference [$ref] is not defined in article '${a.title}'")

def undefId(id: String, a: Article) = {
  println(s"id [$id] is not defined in article '${a.title}'")
  "undefined id "+id
}

def resolveLink(link: String, base: Base, a: Article) = {
  a.linkMap.getOrElse(link, link) match {
    case l if isAbsolute(l) => l
    case l if base.isValidId(l) => absUrlFromSlug(base.find(l).get.slug)
    case l if l matches """[\w/%-]+""" => undefRefError(Seq(link) ++ a.linkMap.get(link) mkString " -> ", a) ; link
    case l => l
  }
}


def getArticle(lines: Vector[String]): (Article, Vector[String]) = {
  val underlinePos = lines.indexWhere(l => l.startsWith("==="), 2)

  if (underlinePos == -1) {
    (parseArticle(lines), Vector())
  } else {
    val (art, rest) = lines.splitAt(underlinePos-1)
    (parseArticle(art), rest)
  }
}


val titleRegex    = """^(XXX+\s*)?(.+?)(?:\[([^ ]+)\])?$""".r
val dateRegex     = """^(\d+)-(\d+)-(\d+)(?: (\d+):(\d+)(?::(\d+))?)?$""".r
val tagsRegex     = """^((?:#|\?|!).+)$""".r

val linkRefRegex      = """(?xm)      ^\[(.*?)\]:\ (.+)$""".r
val boldRegex     = """(?xs)\*\*(.+?)\*\*""".r
val italicRegex   = """(?xsUu) (?<!\*) \* (?!\*) (.+?) (?<!\*) \* (?!\*) """.r
val italic2Regex  = """(?xsUu) (?<!:) // (?=\b|\S) (.+?) (?<!:) (?<=\b|\S) // """.r
val headingRegex  = """(?xm) ^ ([^\n]+) \n ---+""".r
val hrRegex       = """(?xm) ---+ """.r
val altRegex      = """(?xm) " ([^"]*?) \s+ \.\(  (.*?)  \)" """.r
val linkRegex     = """(?x)  " ([^"]+?) " : \[ ([^\]\n]+?) \]""".r
val ahrefRegex    = """(?x) (?<= href=") (.*?) (?=") """.r
val codeRegex     = """(?xs) /---code[^\n]*\n (.*?) \\---""".r

val imgRegexFragment = """
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

val imgRegex      = s"""(?xm) $imgRegexFragment""".r
val blackoutRegex = """(?xs) \[\|.+?\|\] """.r

val imgLicenseRegex = """(?x) (.*?) \s* (?: \( (?:(CC\ [\w-]+)\s+)? \s* ([^\ ]*)? \) )? \s*$""".r
val licenses = Set("CC by", "CC by-nc", "CC by-nd", "CC by-sa", "CC by-nc-nd", "CC by-nc-sa")

def mkImage: PartialFunction[String, Image] = {
  case imgRegex(url, mod, alt1, alt2, align, link, title) =>
    val u = if (link != null) link else url // ???
    val (t, license, source) = title match {
      case imgLicenseRegex(t, l, s) if licenses.contains(l) => (t, l, s)
      case imgLicenseRegex(t, null, s) => (t, null, s)
      case t => (t, null, null)
    }

    Image(
      url = if (isAbsolute(u)) u else Blog.imageRoot + u,
      thumb = hash(url),
      alt  = if (alt1 != null) alt1 else alt2,
      mods = mod,
      align = if (align == "*") null else align,
      title = t,
      license = license,
      source = source
    )
}

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

def parseLicense(l: String): Option[String] =
  if (licenses.contains(l)) Some(l) else None

val parseTags: PartialFunction[String, Tags] = {
    case tagsRegex(s) =>
      val symbols = Set("#", "!", "?")
      var res = Tags()
      var ts = s.split("(\\s*,)?\\s+")

      while (ts.nonEmpty) {
        val tags = ts.tail.takeWhile(t => !symbols.contains(t))
        res = ts.head match {
          case "!" => res.copy(supertags = res.supertags ++ tags)
          case "#" => res.copy(visible   = res.visible   ++ tags)
          case "?" => res.copy(hidden    = res.hidden    ++ tags)
        }
        ts = ts.drop(tags.length + 1)
      }
      res
  }

def _parseMetaFormat(str: String): Meta = Meta(
  if (str.trim.isEmpty) {
    Map()
  } else {
    val Array(k, rest) = str.split(":", 2)
    if (rest.trim.apply(0) == '[') {
      val Array(v, rest2) = rest.trim.tail.split("]", 2) // 2 levels only
      _parseMetaFormat(rest2.replaceAll("\\s*(,\\s*)", "")).xs + ((k.trim, _parseMetaFormat(v)))
    } else {
      rest.split(",", 2) match {
        case Array(v, rest2) => _parseMetaFormat(rest2).xs + (s"${k.trim}:${v.trim}" -> null)
        case Array(v) => Map(s"${k.trim}:${v.trim}" -> null)
      }
    }
  }
)

def parseMeta(l: String, prefix: String = "meta: "): Option[Meta] =
  if (l.startsWith(prefix)) {
    Some(_parseMetaFormat(l.drop(prefix.length)))
  } else None

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
  val titleRegex(xxx, title, slug) = titleLine
  val realSlug = if (slug == null || slug == "") generateSlug(title) else slug
  val inFeed = xxx == null

  val (metaLines, _body) = ls.drop(2).span(l => l.nonEmpty)
  val body = _body.dropWhile(l => l.isEmpty).reverse.dropWhile(l => l.isEmpty).reverse

  val dates   = metaLines.flatMap(parseDate _      andThen (_.toSeq))
  val tags    = metaLines.flatMap(parseTags.lift   andThen (_.toSeq))
  val license = metaLines.flatMap(parseLicense _   andThen (_.toSeq))
  val meta    = metaLines.flatMap(l => parseMeta(l, "meta: ").toSeq)

  if ((dates ++ tags ++ license ++ meta).size < metaLines.size)
    sys.error("some metainformation was not processed: "+metaLines)

  val txt = segmentText(body.mkString("\n"))
  val linkRefs = txt.linkRefs
  if (linkRefs.map(_._1).toSet.size != linkRefs.size) {
    //sys.error(s"duplicate link refs in article '$realSlug'")
  }
  if (linkRefs.exists { case (r, url) => url.trim.startsWith("???") }) {
    //sys.error(s"undefined linkRef url in article '$realSlug'")
  }

  new Article(
    title   = title.trim,
    slug    = realSlug,
    date    = dates.headOption.map(_.head).getOrElse(null), // TODO, currently it's using only the first date
    tags    = tags.fold(Tags()) { (a, b) => Tags(a.visible ++ b.visible, a.hidden ++ b.hidden, a.supertags ++ b.supertags)},
    license = license.headOption.getOrElse(null),
    rawText = body.mkString("\n"),
    images  = txt.images,
    linkMap = txt.linkRefs.toMap,
    links   = txt.links,
    meta    = meta.headOption.getOrElse(Meta()),
    inFeed  = inFeed
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
    .replaceAll("-+$|^-+", "")
}

def tagSlug(tag: String) = "tag/"+generateSlug(tag)



trait Layout {
  def makePage(content: String, title: String = null, gallery: Boolean = false, rss: String = null): String
  def makeIndex(articles: Base): String
  def makeFullArticle(a: Article, compact: Boolean): String
  def makeTagPage(t: String, as: Seq[Article]): String
  def makeTagIndex(base: Base): String
}


case class Text(segments: Seq[Segment]) {
  def linkRefs: Seq[(String, String)] = segments.collect { case Linkref(lm) => lm }.flatMap { _.iterator }
  def links: Seq[String] = segments.collect {
    case Paragraph(txt) =>
      (ahrefRegex.findAllMatchIn(txt).map(_.group(1)) ++ linkRegex.findAllMatchIn(txt).map(_.group(2))).toVector
    case Blockquote(txt) => txt.links
  }.flatten
  def images: Seq[Image] = segments.collect { case Images(imgs) => imgs }.flatten
}

sealed trait Segment
final case class Heading(txt: String) extends Segment
final case class Hr() extends Segment
final case class Linkref(linkMap: Map[String, String]) extends Segment
final case class Images(images: Seq[Image]) extends Segment
final case class Paragraph(txt: String) extends Segment
final case class Code(txt: String) extends Segment
final case class Blockquote(txt: Text) extends Segment

def segmentText(txt: String): Text = {
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
          }.map { ls => Blockquote(Text(splitBlocks(ls.mkString("\n")))) }
        }.getOrElse {
          Paragraph(txt)
        }
    }

  var prev = 0
  val segments: Seq[Segment] = (for (m <- codeRegex.findAllMatchIn(txt)) yield {
    val res: Seq[Segment] = splitBlocks(txt.substring(prev, m.start)) :+ Code(m.group(1))
    prev = m.end+1
    res
  }).toVector.flatten ++ (if (prev > txt.length) Seq() else splitBlocks(txt.substring(prev)))

  Text(segments)
}


class FlowLayout(baseUrl: String, base: Base) extends Layout {
  def rel(url: String): String = relativize(url, baseUrl)
  def txl(s: String) = Blog.translation(s)
  def ifs(c: Boolean, body: => String) = if (c) body else ""
  def ifs(x: Any, body: => String) = if (x != null) body else ""
  def ifs(x: String) = if (x != null) x else ""

  def decorateText(a: Article): String = {
    if (a.format == "html") {
      return a.rawText // TODO - resolveLink
    }

    def paragraph(_txt: String) = {
      var txt = _txt
      txt = blackout(txt)
      txt = altRegex.replaceAllIn(txt, """<span class=about title="$2">$1</span>""")
      txt = ahrefRegex.replaceAllIn(txt, m => rel(resolveLink(m.group(1), base, a)))
      txt = linkRegex.replaceAllIn(txt, m => {
        s"""<a href="${rel(resolveLink(m.group(2), base, a))}">${m.group(1)}</a>"""
      })
      txt = txt.replaceAll("""(?xm) ^(-\ |\ \ )(?!\ )(.*?)(?=\n(?:-\ |\n\n)) """, "$1$2<br/>") // lists
      txt = txt.replaceAll("""(?xs) \<!--.*?--\>""", "")
      txt = boldRegex.replaceAllIn(txt, """<b>$1</b>""")
      txt = italicRegex.replaceAllIn(txt, """<i>$1</i>""")
      txt = italic2Regex.replaceAllIn(txt, """<i>$1</i>""")
      "<p>"+txt+"</p>"
    }

    def mkText(txt: Text): String = {
      var x: Option[Images] = None
      val s1 = txt.segments.map {
        case Images(is) if x == None && is.head.mods == "float_right" =>
          x = Some(Images(Seq(is.head)))
          Images(is.tail)
        case s => s
      }

      val s2 = (x ++ s1).toVector match {
        case xs :+ Hr() => xs :+ Hr()
        case xs => xs :+ Hr()
      }

      s2.map {
        case Heading(txt)   => s"<h3>$txt</h3>"
        case Hr()           => "<hr/>"
        case Linkref(txt)   => ""
        case Code(txt)      => s"<pre>$txt</pre>"
        case Images(images) =>
          images.map {
            case i if i.mods == "main" => imgTag(i, "main", true)
            case i if i.mods == "float_right" => imgTag(i, "fr", true)
            case i => imgTag(i, "thz")
          }.mkString(" ")
        case Paragraph(txt) => paragraph(txt)
        case Blockquote(txt) => "<blockquote>"+mkText(txt)+"</blockquote>"
      }.mkString("")//+"<br/>"
    }

    mkText(segmentText(a.rawText))
  }

  def imgTag(img: Image, cl: String, full: Boolean = false) = {
    val thumbPath = thumbnailUrl(img)
    val desc =
      (ifs(img.title)+" "+
      (ifs(img.license)+" "+ifs(img.source, s"""(<a href="${img.source}">${txl("source")}</a>)""")).trim).trim
    s"""<span class=$cl><a href="${img.url}"><img class=thz title="${ifs(img.alt)}" src="${if (full) img.url else rel(thumbPath)}"/></a>$desc</span>"""
  }

  def makePage(content: String, title: String = null, gallery: Boolean = false, rss: String = null): String = {
s"""<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8" />
<title>${(if (title != null) title+" | " else "")+Blog.title}</title>
<link rel="alternate" type="application/rss+xml" href="${rel("rss.xml")}"/>
${if (rss != null) s"""<link rel="alternate" type="application/rss+xml" href="${rel(rss)}"/>""" else ""}
<style>a{color:inherit}blockquote{margin:0;padding:0;font-style:italic;}.r{text-align:right}.f{float:right}.b{max-width:46em;font-family:monospace;line-height:1.3}.about{text-decoration: underline red;}
img.th,img.thz{}
.thz,.fr,.main{font-size:0.8em}
span.thz {width:${Blog.thumbWidth}px;display:inline-block;vertical-align:top}
span.fr {max-width:50%;float:right;text-align:right;} span.fr img {max-width:100%}
span.main {display:block;text-align:right;margin-bottom:0.5em;} span.main img {max-width:100%}
h2 {display:inline;margin:none;font-size:1em }
hr { border: 0px dashed gray; border-top-width: 1px; margin: 0.5em 4em; }
p { margin: 1.5em 0; }
${Blog.style}</style>
${if (gallery) { s"<script>$galleryScript</script>" } else ""}
</head>
<body>
<div class=b>
<div class=r><b><a href="${rel("index.html")}">${Blog.title}</a></b> [<a href="${rel("rss.xml")}">RSS</a>]</div>
"""+content+"""
</div>
</body>
</html>"""
  }

  def makeIndex(articles: Base): String = {
    val (fulls, links) = articles.feed.splitAt(Blog.articlesOnIndex)
    fulls.map(a => makeFullArticle(a, true)).mkString("<br/><br/>\n") ++ links.map(makeLink).mkString("<br/>\n") + "<br/>"
  }

  def makeFullArticle(a: Article, compact: Boolean): String = {
    makeTitle(a)+
    ifs(!compact, "<span class=f>"+makeNextPrevArrows(a)+"</span>")+
    "<br/>\n"+
    decorateText(a)+
    ifs(!compact,
      "<div style='font-size:0.9em;'>"+
      "<div class='f r'>"+
        ifs(a.tags.supertags.nonEmpty, makeSupertagLinks(a.tags.supertags, a)+"<br/>")+
        ifs(a.tags.visible.nonEmpty,   makeTagLinks(a.tags.visible)+"<br/>")+
        ifs(a.license != null, a.license+"</br>")+
      "</div>"+
      makeNextPrevLinks(a)+
      ifs(a.pub.nonEmpty, txl("published")+"<br/>"+ a.pub.map(makeLink).mkString("<br/>")+"<br/>")+
      ifs(a.pubBy != null, txl("publishedBy")+" "+articleLink(a.pubBy, makeDate(a))+"<br/>")+
      ifs(a.backlinks.nonEmpty || a.similar.nonEmpty,
        txl("similar")+"<br/> "+(a.similar ++ a.backlinks).map(s => articleLink(s, s.title)).mkString("<br/>")+"<br/>"
      )+
    "</div>")
  }

  def makeTagPage(t: String, as: Seq[Article]) = {
    makeTagLink(t)+"<br/><br/>"+
    base.find(tagSlug(t)).map(a => decorateText(a)).getOrElse("")+
    as.map(makeLink).mkString("<br/>")
  }

  def makeTagIndex(base: Base) = // TODO difference between tags and supertags
    base.tagMap.toSeq.sortBy(~_._2.size).map { case (t, as) => makeTagLink(t)+" ("+as.size+")" }.mkString(" ")

  def blackout(txt: String) =
    blackoutRegex.replaceAllIn(txt, m => m.group(0).replaceAll("(?s).", "█").grouped(5).mkString("<wbr>"))

  def articleLink(a: Article, title: String) = s"""<i><a href="${rel(absUrl(a))}">${title}</a></i>"""
  def makeLink(a: Article) = makeDate(a)+articleLink(a, a.title)
  def makeTitle(a: Article) = makeDate(a)+"<h2>"+articleLink(a, a.title)+"</h2>"

  def makeTagLink(t: String) =
    s"""#<i><a href="${rel(absUrlFromSlug(tagSlug(t)))}">${t}</a></i>"""

  def makeNextPrevLinks(a: Article) =
    (if (base.prev(a) == null) "" else "«« "+makeLink(base.prev(a))+"<br/>") +
    (if (base.next(a) == null) "" else "»» "+makeLink(base.next(a))+"<br/>")

  def makeNextPrevArrows(a: Article) =
    (if (base.prev(a) == null) "&nbsp;&nbsp;&nbsp;" else s"""<a id=prev href="${rel(absUrl(base.prev(a)))}">«««</a>""")+" "+
    (if (base.next(a) == null) "&nbsp;&nbsp;&nbsp;" else s"""<a id=next href="${rel(absUrl(base.next(a)))}">»»»</a>""")

  def makeTagLinks(ts: Seq[String]) =
    ts.map(makeTagLink).mkString(" ")

  def makeSupertagLinks(ts: Seq[String], a: Article) =
    ts.map(t =>
      s"""<b>${makeTagLink(t)}</b>"""+
      (if (base.prev(a, t) != null) s""" <a href="${rel(absUrl(base.prev(a, t)))}">««</a>""" else "")+
      (if (base.next(a, t) != null) s""" <a href="${rel(absUrl(base.next(a, t)))}">»»</a>""" else "")
    ).mkString(" ")

  def year(d: Date) = {
    val calendar = new GregorianCalendar()
    calendar.setTime(d)
    calendar.get(Calendar.YEAR)
  }

  val thisYear = year(new Date)

  def makeDate(a: Article) = a.date match {
    case null => ""
    case d if year(a.date) == thisYear =>
      new SimpleDateFormat("d. M.").format(d)+" "
    case d =>
      new SimpleDateFormat("d. M. yyyy").format(d)+" "
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
  var lines: Vector[String] = Blog.files.flatMap { f =>
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

  def invert[A, B](m: Seq[(A, Seq[B])]): Map[B, Seq[A]] =
    m.flatMap { case (a, bs) => bs.map { b => (b, a) }  }
      .groupBy(_._1)
      .map { case (b, bas) => (b, bas.map(_._2).toVector) }

  val base = Base(articles)
  def refs(a: Article, k: String) = a.meta.seq(k) flatMap base.find

  var tagMap: Map[String, Seq[Article]] =
    invert(articles.map { a => (a, (a.tags.visible ++ a.tags.supertags).distinct) })

  val allTagMap: Map[String, Seq[Article]] =
    invert(articles.map { a => (a, (a.tags.visible ++ a.tags.hidden ++ a.tags.supertags).distinct) })

  val backlinks: Map[Slug, Seq[Article]] =
    invert(articles.map { a => (a, a.links map { l => resolveLink(l, base, a) } filter isLocalLink map extractSlug) })

  val pubsBy: Map[Slug, Article] =
    invert(articles.map { a => (a, refs(a, "pub").map(_.asSlug)) })
      .map { case (k, vs) => (k, vs.sortBy(_.date).head) }

  val sim = new Similarities(allTagMap)

  articles = articles map { a =>
    val bs = backlinks.getOrElse(a.asSlug, Seq())
    val pubBy = pubsBy.getOrElse(a.asSlug, null)

    a.meta.seq("pub") foreach { id =>
      if (!base.isValidId(id)) undefId(id, a)
    }

    a.copy(
      date = if (a.date == null && pubBy != null) pubBy.date else a.date,
      backlinks = bs,
      similar = sim.similarByTags(a, count= 5, without = bs),
      pub = refs(a, "pub"),
      pubBy = pubBy
    )
}

  if (Blog.sortByDate) { // newest first, articles without date last
    val byDate = (a: Article) => ~(if (a.date == null) 0 else a.date.getTime)
    articles = articles.sortBy(byDate)
    tagMap = tagMap.map { case (t, as) => (t, as.sortBy(byDate)) }
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



val (articles, indexArticles, tagMap) =
  Blog.kind match {
    case "blog" =>
      val (as, tm) = prepareBlog()
      val base = Base(as, tm)
      (base, base, tm)

    case "gallery" =>
      val (as, is) = prepareGallery()
      (Base(as), Base(is), Map[String, Seq[Article]]())
    case _ => sys.error("wut")
  }



val fileIndex = mutable.ArrayBuffer[(String, String)]()

// make index
val isIndexGallery = articles.all.take(Blog.articlesOnIndex).exists(_.images.nonEmpty)

val path = "index.html"
val l = new FlowLayout(absUrlFromPath(path), articles)
val content = l.makeIndex(indexArticles)
fileIndex += saveFile(path, l.makePage(content, gallery = isIndexGallery))

// make articles
articles.all foreach { a =>
  val path = relUrlFromSlug(a.slug)
  var l = new FlowLayout(absUrlFromSlug(a.slug), articles)
  val content = l.makeFullArticle(a, false)
  fileIndex += saveFile(path, l.makePage(content, title = a.title, gallery = a.images.nonEmpty))
}

// make tag pages
tagMap foreach { case (t, as) =>
  val path = relUrlFromSlug(tagSlug(t))
  var l = new FlowLayout(absUrlFromSlug(tagSlug(t)), articles)
  val content = l.makeTagPage(t, as)
  fileIndex += saveFile(path, l.makePage(content, title = t, rss = tagSlug(t)+".xml"))
  fileIndex += saveXml(tagSlug(t), generateRSS(as))
}

{
  val path = "tags.html"
  val l = new FlowLayout(absUrlFromPath(path), articles)
  val content = l.makeTagIndex(articles)
  fileIndex += saveFile(path, l.makePage(content))
}

// make RSS
fileIndex += saveXml("rss", generateRSS(articles.feed))


// make thumbnails
val images = articles.all.flatMap(_.images)
new File("t").mkdir()
for (image <- images) {
  val (w, h) = (Blog.thumbWidth, Blog.thumbHeight)
  val thumbFile = new File(thumbnailUrl(image))
  if (!thumbFile.exists) {
    println(s"resizing image ${image.url} -> $thumbFile")
    try {
      val full = ImageIO.read(new URL(fixPath(image.url)))
      if (full != null) {
        ImageIO.write(resizeImage(full, w, h), "jpg", thumbFile)
      } else {
        println(s"ImageIO.read(${image.url}) == null")
      }
    } catch { case e: javax.imageio.IIOException =>
      println(e)
    }
  }
}

// make robots.txt
fileIndex += saveFile("robots.txt", "User-agent: *\nAllow: /")

// make file index
saveFile(".files", fileIndex.map { case (file, hash) => file+" "+hash }.mkString("\n"))
