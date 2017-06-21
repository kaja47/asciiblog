import java.awt.image.{ BufferedImage, ConvolveOp, Kernel }
import java.awt.{ AlphaComposite, RenderingHints => RH }
import java.io.{ File, FileWriter }
import java.net.{ URL, URI, HttpURLConnection }
import java.text.SimpleDateFormat
import java.util.{ Date, GregorianCalendar, Calendar, Locale }
import javax.imageio.ImageIO
import scala.collection.{ mutable, immutable }
import scala.util.matching.Regex


val cfg = io.Source.fromFile(args(0)).getLines.collect(kv).toMap

def kv: PartialFunction[String, (String, String)] = {
  case s if s.split(" ", 2).length == 2 =>
    val Array(k, v) = s.split(" ", 2); (k, v)
}

def timer[T](label: String)(f: => T) = {
  val s = System.currentTimeMillis
  val res = f
  println(s"$label ${System.currentTimeMillis-s} ms")
  res
}

val thisDir =
  new File(System.getProperty("sun.java.command").split(" ").find(_.endsWith("makefiles.scala")).get).getParent

val galleryScript =
  io.Source.fromFile(thisDir+"/gallery.js").mkString
    .replaceAll("(?<!let|function|in)[\\s]+(?!in)|/\\*.*?\\*/", "") // rather crude and incorrect minifier

object Blog {
  val kind: String           = cfg.getOrElse("type", "blog")
  val title: String          = cfg("title")
  val baseUrl: String        = cfg("baseUrl")
  val files: Seq[String]     = spaceSeparatedStrings(cfg.getOrElse("files", "").trim)
  val articlesOnIndex: Int   = cfg.getOrElse("fullArticlesOnIndex", "5").toInt
  val style: String          = cfg.getOrElse("style", "")
  val thumbWidth: Int        = cfg.getOrElse("thumbnailWidth", "150").toInt
  val thumbHeight: Int       = cfg.getOrElse("thumbnailHeight", "100").toInt
  val bigThumbWidth: Int     = cfg.getOrElse("bigThumbnailWidth", "800").toInt
  val limitRss: Int          = cfg.getOrElse("limitRss", Int.MaxValue.toString).toInt
  val articlesInRss: Boolean = cfg.getOrElse("articlesInRss", "false").toBoolean
  val sortByDate: Boolean    = cfg.getOrElse("sortByDate", "false").toBoolean
  val imageRoot: String      = cfg.getOrElse("imageRoot", "")
  val articlesMustBeSorted: Boolean = cfg.getOrElse("articlesMustBeSorted", "false").toBoolean
  val articlesMustNotBeMixed: Boolean = cfg.getOrElse("articlesMustNotBeMixed", "false").toBoolean
  val translation: Map[String, String] = io.Source.fromFile(thisDir+"/lang.cs").getLines.collect(kv).toMap
  val dumpAll: Boolean       = cfg.getOrElse("dumpAll", "false").toBoolean // ignore hidden articles, dump everything into main feed
  val header: String         = cfg.getOrElse("header", "")
  val compressFiles: Boolean = cfg.getOrElse("compressFiles", "false").toBoolean
}

def spaceSeparatedStrings(str: String): Seq[String] = str match {
  case "" => Seq()
  case str if str(0) == '"' =>
    val (s, rest) = str.drop(1).span(_ != '"')
    s +: spaceSeparatedStrings(rest.drop(1).trim)
  case _ =>
    val (s, rest) = str.span(_ != ' ')
    s +: spaceSeparatedStrings(rest.trim)
}



val patternBracketRegex = """(?x) ^(.*?)\{(.*)\}$ """.r
def listFiles(pattern: String): Array[File] = ((pattern match {
  case p if p.endsWith("*") =>
    val f = new File(p.init)
    if (f.isDirectory) {
      val fs = f.listFiles
      if (fs == null) Array() else fs
    } else {
      val prefix = f.getName
      val fs = f.getParentFile.listFiles
      if (fs == null) Array() else fs.filter { _.getName.startsWith(prefix) }
    }
  case patternBracketRegex(p, variants) =>
    val f = new File(p)
    if (f.isDirectory) {
      variants.split(",").map { v => new File(f, v) }
    } else {
      variants.split(",").map { v => new File(f.getParentFile, f.getName+v) }
    }
  case _ =>
    val f = new File(pattern)
    if (f.isDirectory) f.listFiles else Array(f)
  }): Array[File]).filter(f => !f.getName.startsWith("."))


case class Base(all: Seq[Article], tagMap: Map[Tag, Seq[Article]] = Map()) {
  private lazy val extraTags: Seq[Article] = {
    val direct = all.collect { case a if a.isTag => a.asTag }.toSet
    tagMap.collect { case (t, as) if !direct.contains(t) => Article(t.title, tagSlug(t.title), meta = Meta(Map("tag" -> null))) }.toSeq
  }

  lazy val bySlug: Map[String, Article] = (all ++ extraTags).map(a => (a.slug, a)).toMap
  lazy val byMeta: Map[String, Article] = (all ++ extraTags).flatMap(a => a.meta.scalars collect { case m: String => (m, a) }).toMap

  lazy val articles = all.filter(a => !a.isTag)
  lazy val feed = all.filter(a => a.inFeed && !a.isTag)

  lazy val allTags: Map[Article, Seq[Article]] =
    (all ++ extraTags).filter(_.isTag).map { t => (t, tagMap.getOrElse(t.asTag, Seq())) }.toMap

  lazy val tagByTitle: Map[Tag, Article] = allTags.map { case (t, _) => (t.asTag, t) }

  private lazy val art2ord = feed.zipWithIndex.toMap

  def find(id: String): Option[Article] = bySlug.get(id).orElse(byMeta.get(id))
  def isValidId(id: String) = find(id).nonEmpty
  def canonicSlug(id: String) = find(id).get.slug

  def next(a: Article) = art2ord.get(a).flatMap { ord => feed.lift(ord+1) }.getOrElse(null)
  def prev(a: Article) = art2ord.get(a).flatMap { ord => feed.lift(ord-1) }.getOrElse(null)

  private def move(a: Article, tag: Tag, n: Int) = {
    val as = tagMap(tag)
    val pos = as.indexWhere(_.slug == a.slug)
    require(pos != -1)
    if (pos+n < 0 || pos+n >= as.length) null
    else as(pos+n)
  }

  def next(a: Article, tag: Tag) = move(a, tag, +1)
  def prev(a: Article, tag: Tag) = move(a, tag, -1)
}

class Similarities(base: Base, tagMap: Map[Tag, Seq[Article]]) {
  private val arts: Array[Article] = tagMap.values.flatten.toArray.distinct
  private val artMap: Map[Article, Int] = arts.zipWithIndex.toMap
  private val tm: Map[Tag, Array[Int]] = tagMap.map { case (t, as) => (t, as.map(artMap).toArray) }

  def similarByTags(a: Article): Seq[Sim] = {
    def dateDiff(a: Article, b: Article): Long = {
      (a.date, b.date) match {
        case (null, null) => 0
        case (null, _) | (_, null) => Long.MaxValue/2
        case (a, b) => math.abs(a.getTime - b.getTime)
      }
    }
    val freq = new Array[Int](arts.length) // article idx -> count
    for (t <- (a.tags.visible ++ a.tags.hidden); arr <- tm.get(t)) {
      var i = 0; while (i < arr.length) {
        freq(arr(i)) += 1
        i += 1
      }
    }

    for (i <- a.meta.seq("rel").flatMap(base.find).flatMap(artMap.get)) {
      freq(i) += 64
    }

    (0 until arts.length).iterator.collect { case i if freq(i) >= 1 && arts(i).slug != a.slug =>
      Sim(arts(i), freq(i))
    } .toVector
      .sortBy { s => (~s.commonTags, dateDiff(a, s.article)) } // most common tags, published closest together
  }

  def similarByTags(a: Article, count: Int, without: Seq[Article]): Seq[Article] =
    similarByTags(a).filter(s => !without.contains(s.article)).take(count).map(_.article)
}

case class Article(
  title: String,
  slug: String,
  dates: Seq[Date] = Seq(), // publishing date + dates of updates
  tags: Tags = Tags(),
  meta: Meta = Meta(),
  license: String = null,
  rawText: String = "",
  images: Seq[Image] = Seq(),
  linkMap: Map[String, String] = Map(), // map of link refs only
  links: Seq[String] = Seq(), // all link bodies (inside "":[ref] or href="url")
  backlinks: Seq[Article] = Seq(),
  similar: Seq[Article] = Seq(),
  pub: Seq[Article] = Seq(),
  pubBy: Article = null,
  inFeed: Boolean = true
) {
  def date = dates.headOption.getOrElse(null)
  def prettyDate = if (date == null) "" else new SimpleDateFormat("MM-dd-yyyy").format(date)
  override def toString = (if (isTag) "Article[Tag]" else "Article")+s"(<$prettyDate>$title)"
  def asSlug: Slug = Slug(slug)
  def format = meta.scalar("format")
  def isSupertag = meta.values.contains("supertag")
  def isTag      = meta.values.contains("tag") || isSupertag
  def asTag = if (isTag) Tag(title, isSupertag) else null
}

case class Meta(values: Map[String, Meta] = Map()) {
  def scalar(key: String): String = scalars.find(_.startsWith(key+":")).map(_.drop(key.length+1).trim).getOrElse(null)
  def scalars = values.collect { case (k, v) if v == null => k }
  def seq(k: String): Seq[String] = values.get(k).map(_.values.keys.toSeq).getOrElse(Seq())
  def merge(that: Meta): Meta =
    Meta(that.values.foldLeft(values) { case (res, (k, v)) =>
      res.get(k) match {
        case None => res + ((k, v))
        case Some(m) => res + ((k, v.merge(m)))
      }
    })
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
case class Tags(visible: Seq[Tag] = Seq(), hidden: Seq[Tag] = Seq()) {
  def merge(t: Tags) = Tags(visible ++ t.visible, hidden ++ t.hidden)
}
case class Tag(title: String, supertag: Boolean = false)
case class Sim(article: Article, commonTags: Int)

def resizeImage(src: BufferedImage, _width: Int, _height: Int = -1): BufferedImage = {
  val (width, height) =
    if (_height <= 0) {
      if (_width > src.getWidth) (src.getWidth, src.getHeight) // do not scale up
      else (_width, (1.0 * _width / src.getWidth * src.getHeight).toInt)
    } else (_width, _height)
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
def relUrlFromSlug(slug: String) = slug + ".html"
def absUrl(a: Article) = absUrlFromSlug(a.slug)
def relUrl(a: Article) = relUrlFromSlug(a.slug)
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
def slugsOfLinkedArticles(a: Article, base: Base): Seq[Slug] =
    a.links.map(l => resolveLink(l, base, a)).filter(isLocalLink).map(extractSlug)
def thumbnailUrl(img: Image) = s"t/${img.thumb}-${Blog.thumbWidth}x${Blog.thumbHeight}"
def bigThumbnailUrl(img: Image, half: Boolean) = s"t/${img.thumb}-${Blog.bigThumbWidth / (if (half) 2 else 1)}"

def undefRefError(ref: String, a: Article) =
  println(s"link reference [$ref] is not defined in article '${a.title} [${a.slug}]'")

def undefId(id: String, a: Article) = {
  println(s"id [$id] is not defined in article '${a.title}'")
  "undefined id "+id
}

def resolveLink(link: String, base: Base, a: Article) =
  a.linkMap.getOrElse(link, link) match {
    case l if isAbsolute(l) => l
    case l if base.isValidId(l) => absUrlFromSlug(base.canonicSlug(l))
    case l if l matches """[\w/%-]+""" => undefRefError((Seq(link) ++ a.linkMap.get(link)).mkString(" -> "), a) ; link
    case l => l
  }


val titleRegex    = """^(XXX+\s*)?(.+?)(?:\[([^ ]+)\])?$""".r
val dateRegex     = """^(\d+)-(\d+)-(\d+)(?: (\d+):(\d+)(?::(\d+))?)?$""".r

val linkRefRegex  = """(?xm)      ^\[(.*?)\]:\ (.+)$""".r
val boldRegex     = """(?xs)\*\*(.+?)\*\*""".r
val italicRegex   = """(?xsUu) (?<!\*) \* (?!\*) (.+?) (?<!\*) \* (?!\*) """.r
val italic2Regex  = """(?xsUu) (?<!:) // (?=\b|\S) (.+?) (?<!:) (?<=\b|\S) // """.r
val headingRegex  = """(?xm) ^ ([^\n]+) \n ---+""".r
val hrRegex       = """(?xm) ---+|\*\*\*+ """.r
val altRegex      = """(?xm) " ([^"]*?) \s+ \.\(  (.*?)  \)" """.r
val linkRegex     = """(?x)  " ([^"]+?) " : \[ ([^\]\n]+?) \]""".r
val ahrefRegex    = """(?x) (?<= href=") (.*?) (?=") """.r
val blockRegex    = """(?xs) /---(\w+)[^\n]*\n (.*?) \\--- | \<!-- (.*?) --\>  """.r

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

val tagsRegex     = """^((?:#|\?|!).+)$""".r
val tagBlockRegex = """(##|#|\?|!)(.+?)(?=( |^)(##|#|\?|!)|$)""".r
val parseTags: PartialFunction[String, Tags] = {
  case tagsRegex(s) =>
    tagBlockRegex.findAllMatchIn(s).foldLeft(Tags()) { (t, m) =>
      val tags = m.group(2).split("\\s*,\\s*").map(_.trim)
      val (vis, hid): (Seq[Tag], Seq[Tag]) = (m.group(1) match {
        case "!"|"##" => (tags.map(Tag(_, true)), Seq())
        case "#"      => (unbracketed(tags).map(Tag(_)), bracketed(tags).map(Tag(_)))
        case "?"      => (Seq(), tags.map(Tag(_)))
      })
      t.copy(visible = t.visible ++ vis, hidden = t.hidden ++ hid)
    }
}

val bracketRegex  = """^\(([^)]+)\)$""".r
def bracketed(xs: Array[String])   = xs.collect { case bracketRegex(x) => x }
def unbracketed(xs: Array[String]) = xs.filter { case bracketRegex(x) => false ; case _ => true }

val metaRegex = """(?x) \s+ | \[ | \] | , | \w+:\w+ | [\w/-]+ | :""".r

def _parseMeta(str: String) = {
  def build(tokens: Seq[String]): Meta =
    Meta(tokens.dropWhile(_ == ",") match {
      case Seq(k, ":", "[", rest @ _*) =>
        val (v, Seq("]", rest2 @ _*)) = rest.span(_ != "]") // 2 levels only
        Map(k -> build(v)) ++ build(rest2).values
      case Seq(k, ",", rest @ _*) => Map(k -> null) ++ build(rest).values
      case Seq(k) => Map(k -> null)
      case Seq() => Map()
    })

  build(metaRegex.findAllMatchIn(str).map(_.group(0).trim).filter(_.nonEmpty).toVector)
}

def parseMeta(l: String, prefix: String = "meta: "): Option[Meta] =
  if (l.startsWith(prefix)) {
    Some(_parseMeta(l.drop(prefix.length)))
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

  val (metaLines, _body) = ls.drop(2).span(l => l.nonEmpty)
  val body = _body.dropWhile(l => l.isEmpty).reverse.dropWhile(l => l.isEmpty).reverse

  val dates   = metaLines.flatMap(parseDate _      andThen (_.toSeq))
  val tags    = metaLines.flatMap(parseTags.lift   andThen (_.toSeq))
  val license = metaLines.flatMap(parseLicense _   andThen (_.toSeq))
  val metas   = metaLines.flatMap(l => parseMeta(l, "meta: ").toSeq)
  val meta = metas.foldLeft(Meta())(_ merge _)

  val isTag = meta.values.contains("supertag") || meta.values.contains("tag") || (slug != null && isTagSlug(slug))
  val inFeed = Blog.dumpAll || (xxx == null && !isTag)
  val realSlug =
    if (slug != null && slug != "") slug else
      if (isTag) tagSlug(title)
      else generateSlug(title)

  if ((dates ++ tags ++ license ++ metas).size < metaLines.size)
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
    dates   = dates.flatten,
    tags    = tags.fold(Tags()) { (a, b) => a.merge(b) },
    meta    = meta,
    license = license.headOption.getOrElse(null),
    rawText = body.mkString("\n"),
    images  = txt.images,
    linkMap = txt.linkRefs.toMap,
    links   = txt.links,
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
def isTagSlug(tag: String) = tag.startsWith("tag/")



trait Layout {
  def makePage(content: String, title: String = null, gallery: Boolean = false, rss: String = null): String
  def makeIndex(articles: Base): String
  def makeFullArticle(a: Article, compact: Boolean): String
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
final case class Block(tpe: String, txt: String) extends Segment
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
  val segments: Seq[Segment] = (for (m <- blockRegex.findAllMatchIn(txt)) yield {
    val block = if (m.group(1) != null) Block(m.group(1), m.group(2)) else Block("comment", m.group(3))
    val res = splitBlocks(txt.substring(prev, m.start)) :+ block
    prev = m.end
    res
  }).toVector.flatten ++ (if (prev > txt.length) Seq() else splitBlocks(txt.substring(prev)))

  Text(segments)
}

class FlowLayout(baseUrl: String, base: Base, dumpImages: Boolean) extends Layout {
  def rel(url: String): String = if (baseUrl != null) relativize(url, baseUrl) else url
  def txl(s: String) = Blog.translation(s)
  def ifs(c: Boolean, body: => String) = if (c) body else ""
  def ifs(x: Any, body: => String) = if (x != null) body else ""
  def ifs(x: String) = if (x != null) x else ""

  def paragraph(_txt: String, a: Article) = {
    var txt = _txt
    txt = blackout(txt)
    txt = altRegex.replaceAllIn(txt, """<span class=about title="$2">$1</span>""")
    txt = ahrefRegex.replaceAllIn(txt, m => rel(resolveLink(m.group(1), base, a)))
    txt = linkRegex.replaceAllIn(txt, m => {
      s"""<a href="${rel(resolveLink(m.group(2), base, a))}">${m.group(1)}</a>"""
    })
    txt = txt.replaceAll("""(?xm) ^(-\ |\ \ )(?!\ )(.*?)(?=\n(?:-\ |\n\n|\d+\)\ )) """, "$1$2<br/>") // lists
    txt = txt.replaceAll("""(?xm) ^(\d+\)\ )(?!\ )(.*?)(?=\n(?:\d+\)\ |\n\n)) """, "$1$2<br/>") // lists
    txt = txt.replaceAll("""(?xs) \<!--.*?--\>""", "")
    txt = boldRegex.replaceAllIn(txt, """<b>$1</b>""")
    txt = italicRegex.replaceAllIn(txt, """<i>$1</i>""")
    txt = italic2Regex.replaceAllIn(txt, """<i>$1</i>""")
    txt
  }

  def decorateText(a: Article): String = {
    if (a.format == "html") {
      return a.rawText // TODO - resolveLink
    }

    def mkText(txt: Text): String =
      txt.segments.map {
        case Heading(txt)   => "<h3>"+paragraph(txt, a)+"</h3>"
        case Hr()           => "<hr/>"
        case Linkref(txt)   => ""
        case Block("html", txt) => txt
        case Block("div",  txt) => s"<div>$txt</div>"
        case Block("code", txt) => s"<pre>$txt</pre>"
        case Block("pre",  txt) => s"<pre>$txt</pre>"
        case Block("comment",_) => ""
        case Block(tpe, _)  => sys.error(s"unknown block type $tpe")
        case Images(images) => images.map(img => imgTag(img, a)).mkString(" ")
        case Paragraph(txt) => "<p>"+paragraph(txt, a)+"</p>"
        case Blockquote(txt) => "<blockquote>"+mkText(txt)+"</blockquote>"
      }.mkString("")

    val txt = segmentText(a.rawText)

    var x: Option[Images] = None
    val s1 = txt.segments.map {
      case Images(is) if x == None && is.head.mods == "main" && is.head.align == ">" =>
        x = Some(Images(Seq(is.head)))
        Images(is.tail)
      case s => s
    }

    mkText(Text((x ++ s1).toVector match {
      case xs :+ Hr() => xs
      case xs => xs
    }))
  }

  def imgTag(img: Image, a: Article) = {
    val (cl, srcPath) = img match {
      case i if i.mods == "main" && i.align == ">" => ("fr", bigThumbnailUrl(img, true))
      case i if i.mods == "main" => ("main", bigThumbnailUrl(img, false))
      case i => ("thz", thumbnailUrl(img))
    }
    val desc =
      (ifs(img.title, paragraph(img.title, a))+" "+(ifs(img.license)+" "+ifs(img.source, s"""(<a href="${img.source}">${txl("source")}</a>)""")).trim).trim
    s"""<span class=$cl><a href="${img.url}"><img class=thz title="${ifs(img.alt)}" src="${rel(absUrlFromPath(srcPath))}"/></a>$desc</span>"""
  }

  def gallerySample(a: Article) =
    a.images.take(3).map { i =>
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
  def style(styleLine: String, cats: Set[String]): Option[String] = {
    val (selectorList, rule) = styleLine.span(_ != '{')
    val matchingSelectors = selectorList.split(",").map(_.trim).flatMap { selector =>
      val matches = selector.split(" ").forall { s =>
        selectorRegex.findFirstMatchIn(s) match {
          case Some(m) => (Option(m.group(1)) ++ Option(m.group(2))).forall(cats.contains)
          case None => true
        }
      }
      if (matches) Some(selector) else None
    }
    if (matchingSelectors.nonEmpty) Some(matchingSelectors.mkString(",")+rule) else None
  }

  def styles(styleTxt: String, cats: Set[String]) =
    styleTxt.lines.map(_.trim).filter(_.nonEmpty).flatMap(l => style(l, cats)).mkString("")

  def makePage(content: String, title: String = null, gallery: Boolean = false, rss: String = null): String = {
    val defaultHeader = s"""<div class=r><b><a href="${rel("index.html")}">${Blog.title}</a></b> [<a href="${rel("rss.xml")}">RSS</a>]</div>"""
    val header = if (Blog.header.nonEmpty) Blog.header else defaultHeader
    val body = "<body><div class=b>"+header+content+"</div></body>"
    val cats = classesAndTags(body)

s"""<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8" />
<title>${(if (title != null) title+" | " else "")+Blog.title}</title>
<link rel="alternate" type="application/rss+xml" href="${rel("rss.xml")}"/>
${if (rss != null) s"""<link rel="alternate" type="application/rss+xml" href="${rel(rss)}"/>""" else ""}
<style>${styles(s"""
a{color:inherit}
.r{text-align:right}
.f{float:right}
.b{max-width:46em;font-family:monospace;line-height:1.3}
blockquote{margin:0;padding:0;font-style:italic;}
.about{text-decoration: underline red;}
img.th,img.thz{}
.thz,.fr,.main{font-size:0.8em}
span.thz {width:${Blog.thumbWidth}px;display:inline-block;vertical-align:top}
span.fr {text-align:right; max-width:45%; float:right;}
span.main {text-align:right; display:block; margin-bottom:0.5em;}
span.main img, span.fr img {max-width:100%}
h2 {display:inline;margin:none;font-size:1em }
hr { border: 0px dashed gray; border-top-width: 1px; margin: 0.5em 4em; }
p { margin: 1.4em 0; }
""", cats)}
${Blog.style}</style>
${if (gallery) { s"<script>$galleryScript</script>" } else ""}
</head>$body
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
    ifs(a.isTag, {
      val linked = slugsOfLinkedArticles(a, base).toSet
      base.allTags(a).filter(a => !linked.contains(a.asSlug)).map(makeLink).mkString("<br/>")+"<br/>"
    })+
    ifs(dumpImages && !compact, a.images.map(img => imgTag(img, a)).mkString(" "))+
    ifs(dumpImages && compact, gallerySample(a))+
    ifs(!compact,
      "<hr/>"+
      "<div style='font-size:0.9em;'>"+
      "<div class='f r' style='max-width:50%'>"+
        ifs(a.tags.visible.nonEmpty, makeTagLinks(a.tags.visible.sortBy(!_.supertag).map(base.tagByTitle), a)+"<br/>\n")+
        ifs(a.license, a.license+"<br/>")+
      "</div>"+
      makeNextPrevLinks(a)+
      ifs(a.pub.nonEmpty, txl("published")+"<br/>"+ a.pub.map(makeLink).mkString("<br/>")+"<br/>")+
      ifs(a.pubBy != null, txl("publishedBy")+" "+articleLink(a.pubBy, makeDate(a))+"<br/>")+
      ifs(a.similar.nonEmpty,   "<p>"+txl("similar")+"<br/>"  +a.similar.map(s => articleLink(s, s.title)).mkString("<br/>")  +"</p>")+
      ifs(a.backlinks.nonEmpty, "<p>"+txl("backlinks")+"<br/>"+a.backlinks.map(s => articleLink(s, s.title)).mkString("<br/>")+"</p>")+
    "</div>")
  }

  def makeTagIndex(base: Base) =
    base.allTags.toSeq.sortBy(~_._2.size).map { case (t, as) => makeTagLink(t)+" ("+as.size+")" }.mkString(" ")

  def blackout(txt: String) =
    blackoutRegex.replaceAllIn(txt, m => m.group(0).replaceAll("(?s).", "█").grouped(5).mkString("<wbr>"))

  def articleLink(a: Article, title: String) = s"""<i><a href="${rel(absUrl(a))}">${title}</a></i>"""
  def makeLink(a: Article) = makeDate(a)+articleLink(a, a.title)
  def makeTitle(a: Article) = makeDate(a)+"<h2>"+articleLink(a, a.title)+"</h2>"

  def makeTagLink(t: Article) = {
    val html = s"""#<i><a href="${rel(absUrlFromSlug(t.slug))}">${t.title}</a></i>"""
    if (t.isSupertag) s"<b>$html</b>" else html
  }

  def makeNextPrevLinks(a: Article) =
    ifs(base.prev(a), "«« "+makeLink(base.prev(a))+"<br/>") +
    ifs(base.next(a), "»» "+makeLink(base.next(a))+"<br/>")

  def makeNextPrevArrows(a: Article) =
    (if (base.prev(a) == null) "&nbsp;&nbsp;&nbsp;" else s"""<a id=prev href="${rel(absUrl(base.prev(a)))}">«««</a>""")+" "+
    (if (base.next(a) == null) "&nbsp;&nbsp;&nbsp;" else s"""<a id=next href="${rel(absUrl(base.next(a)))}">»»»</a>""")

  def makeTagLinks(tags: Seq[Article], a: Article) =
    tags.map { t =>
      makeTagLink(t) + ifs(t.isSupertag,
        ifs(base.prev(a, t.asTag), s""" <a href="${rel(absUrl(base.prev(a, t.asTag)))}">««</a>""")+
        ifs(base.next(a, t.asTag), s""" <a href="${rel(absUrl(base.next(a, t.asTag)))}">»»</a>""")
      )
    }.mkString(" ")

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

def makeRSS(articles: Seq[Article], mkBody: Article => String): String =
  "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n" + (
<rss version="2.0">
<channel>
<title>{Blog.title}</title>{
if (mkBody == null)
  articles.map(a => <item><title>{a.title}</title><guid isPermaLink="true">{absUrlFromSlug(a.slug)}</guid><pubDate>{rssdate(a.date)}</pubDate></item>)
else
  articles.map(a => <item><title>{a.title}</title><guid isPermaLink="true">{absUrlFromSlug(a.slug)}</guid><pubDate>{rssdate(a.date)}</pubDate><description>{mkBody(a)}</description></item>)
}</channel>
</rss>).toString



def saveFile(f: String, content: String): Seq[(String, String)] = {
  val p = new File(f).getParentFile
  if (p != null) p.mkdirs()

  val fw = new FileWriter(f)
  fw.write(content)
  fw.close()

  val h = hash(content)

  Seq(f -> h) ++ (if (!Blog.compressFiles) Seq() else {
    val gzf = f+".gz"
    val out = new java.util.zip.GZIPOutputStream(new java.io.FileOutputStream(gzf))
    out.write(content.getBytes("utf-8"))
    out.close()
    Seq(gzf -> h)
  })
}

def saveXml(f: String, content: String): Seq[(String, String)] =
  saveFile(f+".xml", content)




def prepareBlog(): Base = {
  var articles = Blog.files.flatMap(listFiles).flatMap { f =>
    var ls = io.Source.fromFile(f).getLines.toVector
    val starts = ls.zipWithIndex.collect { case (l, i) if l.matches("===+") => i-1 }

    (0 until starts.length).map { i =>
      parseArticle(ls.slice(starts(i), starts.lift(i+1).getOrElse(ls.length)))
    }.toVector
  }

  if (Blog.articlesMustNotBeMixed) {
    val (hidden, rest1) = articles.span { a => a.title.startsWith("?") }
    val (visible, rest) = rest1.span { a => !a.title.startsWith("?") }
    if (rest.nonEmpty) sys.error("hidden and visible articles are mixed up")
  }

  val hiddenSlugs: Set[Slug] = if (Blog.dumpAll) Set() else {
    articles.filter(_.title.startsWith("?")).map(_.asSlug).toSet // this is done ahead of time beucase of article merging
  }

  for (a <- articles) {
    if (hiddenSlugs.contains(a.asSlug) && a.dates.nonEmpty)
      sys.error(s"hidden and dated article are sharing the same slug '${a.slug}', this is most likely an error")
  }

  val now = new Date
  articles = articles.filter { a =>
    val isInPast = a.date == null || a.date.before(now)
    !hiddenSlugs.contains(a.asSlug) && isInPast
  }

  // slug dupes and article merging
  val articleMap = mutable.Map[String, Article]()
  val slugOrder  = mutable.ArrayBuffer[String]()
  for (a <- articles) {
    if (!articleMap.contains(a.slug)) {
      articleMap += ((a.slug, a))
      slugOrder  += a.slug

    } else {
      val b = articleMap(a.slug)

      if (
        (a.dates.nonEmpty && b.dates.nonEmpty) ||
        (a.tags != Tags() && b.tags != Tags()) ||
        (a.license != null && b.license != null) ||
        (a.meta != Meta() && b.meta != Meta()) //||
        //(a.rawText.nonEmpty && b.rawText.nonEmpty)
      ) sys.error("two conflicting articles with the same slug '"+a.slug+"'")


      val merged = b.copy(
        dates   = a.dates ++ b.dates,
        tags    = a.tags merge b.tags,
        meta    = a.meta merge b.meta,
        license = if (a.license != null) a.license else b.license,
        rawText = if (a.rawText.length < b.rawText.length) a.rawText+"\n\n"+b.rawText else b.rawText+"\n\n"+a.rawText,
        images  = a.images ++ b.images,
        linkMap = a.linkMap ++ b.linkMap,
        links   = a.links ++ b.links,
        inFeed  = a.inFeed && b.inFeed
      ) // backlinks, similar, pub, pubBy not yet populated

      articleMap(a.slug) = merged
    }
  }

  articles = slugOrder.map(articleMap)

  // ordered by date
  if (Blog.articlesMustBeSorted) {
    val dated = articles.filter(_.date != null)
    val ordered = dated == dated.sortBy(~_.date.getTime)
    if (!ordered) sys.error("articles are not ordered by date")
  }

  def invert[A, B](m: Seq[(A, Seq[B])]): Map[B, Seq[A]] =
    m.flatMap { case (a, bs) => bs.map { b => (b, a) }  }
      .groupBy(_._1)
      .map { case (b, bas) => (b, bas.map(_._2).toVector) }

  var tagMap: Map[Tag, Seq[Article]] =
    invert(articles.map { a => (a, (a.tags.visible).distinct) })

  val allTagMap: Map[Tag, Seq[Article]] =
    invert(articles.map { a => (a, (a.tags.visible ++ a.tags.hidden).distinct) })

  val base = Base(articles, tagMap)
  def refs(a: Article, k: String) = a.meta.seq(k) flatMap base.find

  val backlinks: Map[Slug, Seq[Article]] =
    invert(articles.map { a => (a, slugsOfLinkedArticles(a, base)) })
      .map { case (k, as) => (k, as.distinct) }

  val pubsBy: Map[Slug, Article] =
    invert(articles.map { a => (a, refs(a, "pub").map(_.asSlug)) })
      .map { case (k, vs) => (k, vs.sortBy(_.date).head) }

  val sim = new Similarities(base, allTagMap)

  articles = articles map { a =>
    val bs = backlinks.getOrElse(a.asSlug, Seq())
    val pubBy = pubsBy.getOrElse(a.asSlug, null)

    a.meta.seq("pub") foreach { id =>
      if (!base.isValidId(id)) undefId(id, a)
    }

    a.copy(
      dates = if (a.dates.isEmpty && pubBy != null) pubBy.dates.take(1) else a.dates,
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

  tagMap = tagMap.map { case (t, as) =>
    val tagArticle = base.tagByTitle(t)
    val key = tagArticle.meta.scalar("sortby")

    if (key == null) { // sort by order linked in article (order for << >> navigation)
      val linked = slugsOfLinkedArticles(tagArticle, base).distinct
      val tagged = as.map(_.asSlug)
      val artMap = as.map(a => (a.asSlug, a)).toMap
      (t, ((linked intersect tagged) ++ (tagged diff linked)).map(artMap))

    } else if (key == "title") {
      (t, as.sortBy(_.title))

    } else {
      (t, as.sortBy { a =>
        val k = a.meta.scalar(key)
        if (k == null) 0 else ~k.toInt
      })
    }
  }

  Base(articles, tagMap)
}



def prepareGallery(): Base = {
  var dir = args(1)
  val albumDirs = new File(dir, "albums").listFiles.sortBy(_.getName).reverse.toSeq

  val dateTitle = """^(?:(\d+)-(\d+)-(\d+)\s*-?\s*)?(.*)$""".r

  Base(for (albumDir <- albumDirs) yield {
    println(albumDir.getName)

    val dateTitle(y, m, d, t) = albumDir.getName
    val (date, title) =
      if (y == null) (null, t)
      else (new GregorianCalendar(y.toInt, m.toInt-1, d.toInt).getTime, t)

    val urls = albumDir.list collect { case f if f.toLowerCase.endsWith(".jpg") =>
      Blog.baseUrl+"/albums/"+albumDir.getName+"/"+f
    }

    new Article(
      title = if (title.nonEmpty) title else albumDir.getName,
      slug = generateSlug(albumDir.getName),
      dates = Seq(date),
      images = urls.map { url => new Image(url, hash(url)) }
    )
  })
}



val (base, gallery) =
  Blog.kind match {
    case "blog"    => (prepareBlog(), false)
    case "gallery" => (prepareGallery(), true)
    case _ => sys.error("unknown kind of blog (should be either 'blog' or 'gallery')")
  }



val fileIndex = mutable.ArrayBuffer[(String, String)]()
val isIndexGallery = base.feed.take(Blog.articlesOnIndex).exists(_.images.nonEmpty)

val path = "index.html"
val l = new FlowLayout(absUrlFromPath(path), base, gallery)
val body = l.makeIndex(base)
fileIndex ++= saveFile(path, l.makePage(body, gallery = isIndexGallery))

base.articles foreach { a =>
  var l = new FlowLayout(absUrl(a), base, gallery)
  val body = l.makeFullArticle(a, false)
  fileIndex ++= saveFile(relUrl(a), l.makePage(body, a.title, gallery = a.images.nonEmpty))
}

base.allTags.keys foreach { a =>
  var l = new FlowLayout(absUrl(a), base, gallery)
  val body = l.makeFullArticle(a, false)
  fileIndex ++= saveFile(relUrl(a), l.makePage(body, a.title, gallery = a.images.nonEmpty, rss = a.slug+".xml"))
  fileIndex ++= saveXml(a.slug, makeRSS(base.allTags(a).take(Blog.limitRss), null))
}

{
  val path = "tags.html"
  val l = new FlowLayout(absUrlFromPath(path), base, gallery)
  fileIndex ++= saveFile(path, l.makePage(l.makeTagIndex(base)))
}

def mkBody(a: Article) = (new FlowLayout(null, base, gallery)).makeFullArticle(a, true)
fileIndex ++= saveXml("rss", makeRSS(base.feed.take(Blog.limitRss), if (Blog.articlesInRss) mkBody else null))

new File("t").mkdir()
for (a <- base.all; image <- a.images) {
  val (thumbFile, w, h) =
    if (image.mods == "main" && image.align == ">") {
      (new File(bigThumbnailUrl(image, true)), Blog.bigThumbWidth/2, -1)
    } else if (image.mods == "main") {
      (new File(bigThumbnailUrl(image, false)), Blog.bigThumbWidth, -1)
    } else {
      val (w, h) = (Blog.thumbWidth, Blog.thumbHeight)
      (new File(thumbnailUrl(image)), w, h)
    }

  if (!thumbFile.exists) {
    println(s"resizing image ${image.url} -> $thumbFile")
    try {
      val suffix = image.url.split("\\.").last.toLowerCase
      val s = if (ImageIO.getWriterFileSuffixes.contains(suffix)) suffix else "jpg"
      val full = ImageIO.read(new URL(fixPath(image.url)))
      if (full != null) {
        ImageIO.write(resizeImage(full, w, h), s, thumbFile)
      } else {
        println(s"ImageIO.read(${image.url}) == null")
      }
    } catch { case e: javax.imageio.IIOException =>
      println(e)
    }
  }
}

fileIndex ++= saveFile("robots.txt", "User-agent: *\nAllow: /")
saveFile(".files", fileIndex.map { case (file, hash) => hash+" "+file }.mkString("\n"))
