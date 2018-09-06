package asciiblog

import MakeFiles.{ hash, tagSlug, invert, UrlOps, isAbsolute }
import java.io.{ File, BufferedWriter, OutputStreamWriter, FileOutputStream }
import java.net.{ URL, URI, HttpURLConnection, UnknownHostException }
import java.security.MessageDigest
import java.text.SimpleDateFormat
import java.util.{ Date, GregorianCalendar, Calendar, Locale }
import javax.imageio.{ ImageIO, IIOException }
import scala.collection.mutable
import scala.util.matching.Regex
import java.util.regex.Matcher
import util._

object T { val t = new Timer; def apply[T](f: => T) = t.apply(f) }

object Make extends App {

  val timer = new Timer()
  timer.start()

  if (args.length < 1) {
    println("config file not specified")
    sys.exit()
  }

  val (_, blog, markup, base, resolver) = MakeFiles.init(args)
  MakeFiles.makeFiles(blog, base, markup, resolver)

  timer.end()
  println("total: "+timer)
}


case class Blog (
  val title: String,
  val baseUrl: String,
  val files: Seq[File],
  val outDir: File,
  val articlesOnIndex: Int,
  val groupArchiveBy: String,
  val archiveFormat: String,
  val tagFormat: String,
  val cssStyle: String,
  val cssFile: String,
  val header: String,
  val footer: String,
  val thumbWidth: Int,
  val thumbHeight: Int,
  val bigThumbWidth: Int,
  val limitRss: Int,
  val articlesInRss: Boolean,
  val limitSimilar: Int,
  val sortByDate: Boolean,
  val imageRoot: String,
  val articlesMustBeSorted: Boolean,
  val articlesMustNotBeMixed: Boolean,
  val language: String,
  val dumpAll: Boolean,
  val fileSuffix: String,
  val imageMarker: String,
  val albumsDir: String,
  val allowComments: Boolean,
  val shareLinks: Boolean,
  val demandExplicitSlugs: Boolean,

  val defaultUser: String,
  val openGraph: Boolean,
  val twitterSite: String,
  val twitterCreator: String,

  val args: Array[String],
  val translation: Map[String, String],
  val hooks: Hooks = null,
) extends UrlOps {
  def hasOgTags = twitterSite.nonEmpty || twitterCreator.nonEmpty || openGraph
  def printTimes: Boolean  = false
}


trait Hooks {
  def indexPrepend(base: Base, blog: Blog, layout: Layout, articles: Seq[Article], isMainIndex: Boolean): String
  def afterFirstArticle(base: Base, blog: Blog, layout: Layout, articles: Seq[Article], isMainIndex: Boolean): String
  def fullArticleBottom(base: Base, blog: Blog, layout: Layout, article: Article): String
  def title(base: Base, blog: Blog, layout: Layout, article: Article, compact: Boolean): String
}

class NoHooks extends Hooks {
  def indexPrepend(base: Base, blog: Blog, layout: Layout, articles: Seq[Article], isMainIndex: Boolean): String = ""
  def afterFirstArticle(base: Base, blog: Blog, layout: Layout, articles: Seq[Article], isMainIndex: Boolean): String = ""
  def fullArticleBottom(base: Base, blog: Blog, layout: Layout, article: Article): String = ""
  def title(base: Base, blog: Blog, layout: Layout, article: Article, compact: Boolean): String = null
}


object Blog {
  def populate(cfg: Map[String, String], args: Array[String], translation: Map[String, String], cfgFile: File) = {
    val cfgDirectory = cfgFile.getParentFile
    val inline = if (cfg.contains("inline!")) Seq(cfgFile) else Seq()
    new Blog(
      title                  = cfg("title"),
      baseUrl                = cfg("baseUrl"),
      files                  = inline ++ spaceSeparatedStrings(cfg.getOrElse("files", "").trim).flatMap(f => globFiles(f, cfgDirectory)), // relative paths are relative to config file
      outDir                 = cfg.get("outDir").map(f => newFile(f, cfgDirectory)).getOrElse(null),
      articlesOnIndex        = cfg.getOrElse("fullArticlesOnIndex", "5").toInt,
      groupArchiveBy         = cfg.getOrElse("groupArchiveBy", "year"), // "year", "month" or some number
      archiveFormat          = cfg.getOrElse("archiveFormat", "link").ensuring(f => f == "link" || f == "short"),
      tagFormat              = cfg.getOrElse("tagFormat", "link").ensuring(f => f == "link" || f == "short"),
      cssStyle               = cfg.getOrElse("style", ""),
      cssFile                = cfg.getOrElse("cssFile", ""),
      header                 = cfg.getOrElse("header", ""),
      footer                 = cfg.getOrElse("footer", ""),
      thumbWidth             = cfg.getOrElse("thumbnailWidth", "150").toInt,
      thumbHeight            = cfg.getOrElse("thumbnailHeight", "100").toInt,
      bigThumbWidth          = cfg.getOrElse("bigThumbnailWidth", "800").toInt,
      limitRss               = cfg.getOrElse("limitRss", Int.MaxValue.toString).toInt,
      articlesInRss          = cfg.getOrElse("fullArticlesInRss", "false").toBoolean,
      limitSimilar           = cfg.getOrElse("limitSimilarLinks", "5").toInt,
      sortByDate             = cfg.getOrElse("sortByDate", "false").toBoolean,
      imageRoot              = cfg.getOrElse("imageRoot", ""),
      articlesMustBeSorted   = cfg.getOrElse("articlesMustBeSorted", "false").toBoolean,
      articlesMustNotBeMixed = cfg.getOrElse("articlesMustNotBeMixed", "false").toBoolean,
      language               = cfg.getOrElse("language", "en"),
      dumpAll                = cfg.getOrElse("dumpAll", "false").toBoolean, // ignore hidden articles, dump everything into main feed
      fileSuffix             = cfg.getOrElse("fileSuffix", ".html"),
      imageMarker            = cfg.getOrElse("imageMarker", ""),
      albumsDir              = cfg.getOrElse("albumsDir", ""),
      allowComments          = cfg.getOrElse("allowComments", "false").toBoolean,
      shareLinks             = cfg.getOrElse("shareLinks", "false").toBoolean,
      demandExplicitSlugs    = cfg.getOrElse("demandExplicitSlugs", "false").toBoolean,

      defaultUser            = cfg.getOrElse("defaultUser", null),
      openGraph              = cfg.getOrElse("openGraph", "false").toBoolean,
      twitterSite            = cfg.getOrElse("twitter.site", ""),
      twitterCreator         = cfg.getOrElse("twitter.creator", ""),

      args = args,
      translation = translation,

      hooks                  = Class.forName(cfg.getOrElse("hooks", "asciiblog.NoHooks")).newInstance().asInstanceOf[Hooks]
    )
  }

  val invalidLinkMarker: String = "@@INVALIDLINK@@"

  private def spaceSeparatedStrings(str: String): Seq[String] = str match {
    case "" => Seq()
    case str if str(0) == '"' =>
      val (s, rest) = str.drop(1).span(_ != '"')
      s +: spaceSeparatedStrings(rest.drop(1).trim)
    case _ =>
      val (s, rest) = str.span(_ != ' ')
      s +: spaceSeparatedStrings(rest.trim)
  }
}

case class Article(
  title: String,
  slug: String,
  author: String = null,
  dates: Seq[Date] = Seq(), // publishing date + dates of updates
  tags: Tags = Tags(),
  meta: Meta = Meta(),
  rel: Seq[String] = Seq(),
  pub: Seq[String] = Seq(),
  aliases: Seq[String] = Seq(),
  implies: Seq[Tag] = Seq(), // only for tags
  imgtags: Seq[Tag] = Seq(),
  link: String = null,
  notes: String = null,
  license: String = null,
  rawText: Seq[String] = Seq(),
  text: Text = null,
  images: Seq[Image] = Seq(),
  backlinks: Seq[Article] = Seq(),
  similar: Seq[Article] = Seq(),
  pubArticles: Seq[Article] = Seq(),
  pubBy: Article = null,
  inFeed: Boolean = true
) {
  val date = if (dates.isEmpty) null else dates.head
  def prettyDate = if (date == null) "" else "<"+new SimpleDateFormat("MM-dd-yyyy").format(date)+">"
  override def toString = (if (isTag) "Article[Tag]" else "Article")+s"($prettyDate$title)"
  def asSlug: Slug = Slug(slug)
  def isSupertag = meta.contains("supertag")
  def isTag      = meta.contains("tag") || isSupertag
  def asTag      = if (isTag) Tag(title, isSupertag) else null
  def extraImages = images.filter(!_.inText)
  def slugsOfLinkedArticles(implicit blog: Blog): Seq[Slug] = text.links.filter(blog.isLocalLink).map(blog.extractSlug)

  // image marker is shown only for images that are not from external sources
  def hasImageMarker = images.exists(i => i.source == null || i.source == "")
  def hasTag(t: Tag) = tags.visible.contains(t)

  def mapImages(f: Image => Image) = {
    def mapSegments(ss: Seq[Segment]): Seq[Segment] = ss.map { // TODO segments are impl. detail of AsciiText
      case Images(is)     => Images(is.map(f))
      case Blockquote(ss) => Blockquote(mapSegments(ss))
      case s => s
    }

    copy(
      images = images.map(f),
      text = text match {
        case t: AsciiText => t.overwriteSegments(mapSegments(t.segments)) // TODO this sould not be here
        case t => t
      }
    )
  }

  def addImageTags(tags: Seq[Tag]) =
    mapImages { i => i.copy(tags = i.tags.copy(visible = (i.tags.visible ++ tags).distinct)) }

  def imagesWithoutArticleTags = {
    val ts = if (isTag) Set(asTag) else tags.visible.toSet
    mapImages { i => i.copy(tags = i.tags.copy(visible = i.tags.visible.filter(t => !ts.contains(t)))) }
  }

  def mkRawText = rawText.mkString("\n")
}

case class Meta(values: Seq[String] = Seq()) {
  val kvPairs: Map[String, String] = values.filter(_.indexOf(':') >= 0).map { x => val Array(k, v) = x.split(":", 2); (k, v) }.toMap
  def value(key: String): String = kvPairs.getOrElse(key, null)
  def contains(x: String) = values.contains(x)
  def merge(that: Meta): Meta = Meta(values ++ that.values)
}

case class Image(
  url: String,
  alt: String = null,
  mods: String = null,
  align: String = null,
  title: String = null,
  license: String = null,
  source: String = null,
  tags: Tags = Tags(),
  inText: Boolean = true, // is this image specified in article text or is it part of a gallery
  localSource: Article = null
) {
  def asSmallThumbnail = copy(mods = "", align = "")

  val thumb = {
    val sb = new java.lang.StringBuilder
    var lastSlash = url.lastIndexOf('/')
    if (lastSlash == -1) lastSlash = url.length

    var lastDot   = url.lastIndexOf('.')
    if (lastDot < lastSlash) lastDot = url.length

    var out = false
    var i = 0; while (i < lastSlash) {
      val ch = url.charAt(i)
      if (ch == '/' || ch == '.' || ch == '-') {
        out = true
      } else if (ch == '%' && i < lastSlash-2 && url.charAt(i+1) == '2' && url.charAt(i+2) == '0') {
        i += 2
        out = true
      } else if (out) {
        sb.append(ch)
        out = false
      }
      i += 1
    }
    sb.append("-")
    sb.append(url, lastSlash+1, lastDot)
    sb.append("-")
    sb.append(url.hashCode.toHexString.take(8))
    sb.toString
  }
}

case class Slug(id: String)
case class Tags(visible: Seq[Tag] = Seq(), hidden: Seq[Tag] = Seq()) {
  def merge(t: Tags) = Tags(visible ++ t.visible, hidden ++ t.hidden)
}
case class Tag(title: String, supertag: Boolean = false) {
  override def toString = "Tag("+hashTag+")"
  def hashTag = (if (supertag) "##" else "#")+title
}


case class Base(all: Vector[Article], tagMap: Map[Tag, Seq[Article]] = Map()) {
  def bySlug(s: Slug): Article = bySlug(s.id)
  lazy val bySlug: Map[String, Article] = all.map(a => (a.slug, a)).toMap
  lazy val articles = all.filter(a => !a.isTag)
  lazy val feed     = all.filter(a => !a.isTag && a.inFeed)
  lazy val images = all.sortBy { a => (Option(a.date), a.title) }.reverse
    .flatMap { a => a.images.map(_.copy(inText = false, localSource = a)) }

  lazy val allTags: Map[Tag, (Article, Seq[Article])] = { // [tag -> (article reprsenting this tag, articles tagged by this tag)]
    val imageTagMap: Map[Tag, Seq[(Image, Article)]] = invert(all.flatMap(a => a.images.map(i => ((i, a), i.tags.visible.distinct))))
    def taggedImages(t: Tag) = imageTagMap.getOrElse(t, Seq()).map { case (i, a) => i.copy(inText = false, localSource = a) }

    all.filter(_.isTag).map { t =>
      t.asTag -> (t.copy(images = t.images ++ taggedImages(t.asTag)), tagMap.getOrElse(t.asTag, Seq()))
    }.toMap
  }

  def tagByTitle(t: Tag): Article = allTags(t)._1

  private lazy val slug2ord: Map[String, Int] = feed.map(_.slug).zipWithIndex.toMap

  def next(a: Article): Article = slug2ord.get(a.slug).flatMap { ord => feed.lift(ord+1) }.getOrElse(null)
  def prev(a: Article): Article = slug2ord.get(a.slug).flatMap { ord => feed.lift(ord-1) }.getOrElse(null)

  private def move(a: Article, tag: Tag, n: Int): Article = {
    val as = tagMap(tag)
    val pos = as.indexWhere(_.slug == a.slug)
    require(pos != -1)
    if (pos+n < 0 || pos+n >= as.length) null
    else as(pos+n)
  }

  def next(a: Article, tag: Tag): Article = move(a, tag, +1)
  def prev(a: Article, tag: Tag): Article = move(a, tag, -1)
}


class Similarities(articles: Seq[Article]) {
  private val tagMap: Map[Tag, Seq[Article]] = invert(articles.map { a => (a, (a.tags.visible ++ a.tags.hidden).distinct) })
  private val tags: Map[Tag, Article] = articles.iterator.collect { case a if a.isTag => (a.asTag, a) }.toMap
  private val arts: Array[Article] = articles.toArray
  private val slugMap: Map[Slug, Int] = arts.iterator.zipWithIndex.map { case (k, v) => (k.asSlug, v) }.toMap
  private val tm: Map[Tag, Array[Int]] = tagMap.map { case (t, as) =>
    val idxs = as.map(a => slugMap(a.asSlug)).toArray
    java.util.Arrays.sort(idxs)
    (t, idxs)
  }
  private val reverseRels: Map[Slug, Seq[Slug]] =
    invert(articles.collect { case a if a.rel.nonEmpty => (a.asSlug, a.rel.map(Slug)) })

  def similarByTags(a: Article, count: Int, without: Seq[Article]): Seq[Article] = {
    def dateDiff(a: Article, b: Article): Long = {
      (a.date, b.date) match {
        case (null, null) => 0
        case (null, _) | (_, null) => Long.MaxValue/2
        case (a, b) => Math.abs(a.getTime - b.getTime)
      }
    }

    val arrs = (a.tags.visible ++ a.tags.hidden).map(tm)
    if (arrs.isEmpty && a.rel.isEmpty) return Seq()

    val freq = new Array[Int](arts.length) // article idx -> count
    for (arr <- arrs) {
      var i = 0; while (i < arr.length) {
        freq(arr(i)) += 1
        i += 1
      }
    }

    for (id <- a.rel) freq(slugMap(Slug(id))) += 64
    for (id <- reverseRels.getOrElse(a.asSlug, Seq())) freq(slugMap(id)) += 1

    for (a <- without ; i <- slugMap.get(a.asSlug)) freq(i) = 0
    for (i <- slugMap.get(a.asSlug)) freq(i) = 0

    case class Key(commonTags: Int, dateDiff: Long, idx: Int)
    implicit val o = new Ordering[Key]{
      def compare(a: Key, b: Key): Int = {
        val c1 = java.lang.Integer.compare(b.commonTags, a.commonTags)
        if (c1 != 0) return c1
        val c2 = java.lang.Long.compare(a.dateDiff, b.dateDiff)
        if (c2 != 0) return c2
        java.lang.Integer.compare(a.idx, b.idx)
      }
    }

    val sortedMap = mutable.TreeSet[Key]()
    var min = Key(0, 0, 0)
    var size = 0

    var i = 0; while (i < arts.length) {
      if (freq(i) >= 1 && freq(i) >= min.commonTags) {
        val key = Key(freq(i), dateDiff(a, arts(i)), i) // articles with most tags in common, published closest together
        if (!o.gt(key, min)) {
          sortedMap.add(key)
          if (size < count) {
            size += 1
          } else {
            val last = sortedMap.last
            sortedMap.remove(last)
            min = last
          }
        }
      }
      i += 1
    }

    sortedMap.iterator.map(key => arts(key.idx)).toVector
  }

  def sortBySimilarity(bs: Seq[Article], a: Article): Seq[Article] = {
    val atags = a.tags.visible.toSet
    def timeOf(a: Article) = if (a.date != null) a.date.getTime else 0
    def commonTags(b: Article) = b.tags.visible.count { bt => atags.contains(bt) }
    bs.map { b => (b, (~commonTags(b), ~timeOf(b), b.slug)) }.sortBy(_._2).map(_._1)
  }

  def similarTags(t: Tag, count: Int): Seq[Article] = {

    class TopN[T: Ordering](n: Int, var min: Double = Double.MinValue) {
      private val set = mutable.TreeSet[(Double, T)]()
      def toSeq = set.toVector

      def += (x: (Double, T)): Unit = {
        if (x._1 < min) return
        set += x
        if (set.size > n) {
          val h = set.head
          min = h._1
          set.remove(h)
        }
      }
    }

    def intersectionSize(a: Array[Int], b: Array[Int]): Int = {
      var size, ai, bi = 0
      while (ai < a.length && bi < b.length) {
        val av = a(ai)
        val bv = b(bi)
        size += (if (av == bv) 1 else 0)
        ai   += (if (av <= bv) 1 else 0)
        bi   += (if (av >= bv) 1 else 0)
      }
      size
    }

    if (!tm.contains(t)) return Seq()

    val idxs = tm(t)
    val topn = new TopN[Tag](count, 0.001 /* zero is never added */)(Ordering.by { t => t.title })

    for ((t2, idxs2) <- tm if t2 != t) {
      val maxSim = 1.0 * Math.min(idxs.length, idxs2.length) / Math.max(idxs.length, idxs2.length)
      if (maxSim >= topn.min) {
        val in = intersectionSize(idxs, idxs2)
        val un = idxs.size + idxs2.size - in
        topn += (in.toDouble / un, t2)
      }
    }

    topn.toSeq.map { case (_, t) => tags(t) }
  }
}



object MakeFiles {

  def keyVal: PartialFunction[String, (String, String)] = {
    case s if s.split(" ", 2).length == 2 =>
      val Array(k, v) = s.split(" ", 2); (k, v)
    case s if !s.contains(" ") && s.endsWith("!") =>
      (s, "")
  }

  private val thisDir: File = {
    val dot = new File(".")
    val cp  = new File(System.getProperty("java.class.path")).getParentFile
    if (new File(dot, "gallery.js").exists()) dot else cp
  }

  private def file(f: String) = new File(thisDir, f)

  private def crudelyMinify(js: String) = js.replaceAll("(?<!let|function|in)[\\s]+(?!in)|/\\*.*?\\*/|//.*\n", "")
  def keyValuesIterator(f: File) = io.Source.fromFile(f).getLines.collect(keyVal)
  def keyValuesMap(f: File) = keyValuesIterator(f).toMap

  lazy val galleryScript  = crudelyMinify(io.Source.fromFile(file("gallery.js")).mkString)
  lazy val commentsScript = io.Source.fromFile(file("comments.php")).mkString
  lazy val outScript      = io.Source.fromFile(file("out.php")).mkString

  def readConfig(cfgFile: File): Map[String, String] = {
    var inlineText = false
    val _cfg = mutable.Map[String, String]()
    val iter = keyValuesIterator(cfgFile)
    while (iter.hasNext) {
      val (k, v) = iter.next()
      if (k.endsWith("|")) {
        val kk = k.init
        _cfg(kk) = (if (_cfg.contains(kk)) _cfg(kk)+"\n"+v else v)
      } else {
        _cfg(k) = v
      }
      if (k == "inline!") return _cfg.toMap
    }
    _cfg.toMap
  }

  def init(args: Array[String]) = {
    val cfgFile = new File(args(0))
    val cfg = readConfig(cfgFile)
    val txl = keyValuesMap(file("lang."+cfg.getOrElse("language", "en")))
    val blog = Blog.populate(cfg, args, txl, cfgFile)
    val markup = AsciiMarkup
    val (newBlog, base, resolver) = makeBase(blog, markup)
    (cfg, newBlog, markup, base, resolver)
  }


  def invert[A, B](m: Seq[(A, Seq[B])]): Map[B, Seq[A]] = {
    val res = mutable.Map[B, mutable.ArrayBuffer[A]]()
    for ((a, bs) <- m; b <- bs) { res.getOrElseUpdate(b, new mutable.ArrayBuffer[A]) += a }
    res.iterator.map { case (b, as) => (b, as.toVector) }.toMap
  }

  def invert1[A, B](m: Seq[(A, Seq[B])]): Map[B, A] = {
    val res = mutable.Map[B, A]()
    for ((a, bs) <- m; b <- bs) { res.getOrElseUpdate(b, a) }
    res.toMap
  }


  def isAbsolute(url: String) = url.startsWith("http") && new URI(url).isAbsolute
  def addParam(url: String, param: String) =
    if (url.contains("?")) url+"&"+param
    else                   url+"?"+param
  def imgSuffix(img: Image) = {
    val imageSuffixes = Set("jpg", "jpeg", "png")
    val suffix = img.url.split("\\.").last.toLowerCase
    if (imageSuffixes.contains(suffix)) "."+suffix else ""
  }

  trait UrlOps { blog: Blog =>
    def absUrlFromPath(path: String) = blog.baseUrl + "/" + path
    def absUrlFromSlug(slug: String) = blog.baseUrl + "/" + slug + blog.fileSuffix
    def relUrlFromSlug(slug: String) = slug + blog.fileSuffix
    def absUrl(a: Article) = absUrlFromSlug(a.slug)
    def relUrl(a: Article) = relUrlFromSlug(a.slug)

    // url might be pretty much anything
    // baseUrl must be some url from this blog
    def relativize(url: String, baseUrl: String): String = {
      if (baseUrl == null || url.startsWith("#")) return url

      // fast path for absolute foreign urls
      if ((url.startsWith("https://") || url.startsWith("http://")) && !url.startsWith(blog.baseUrl)) {
        return url
      }

      // fast path
      if (url.startsWith(blog.baseUrl) && baseUrl.startsWith(blog.baseUrl)) {
        val len = Math.min(url.length, baseUrl.length)
        var prefixLength = 0
        var i = blog.baseUrl.length; while (i < len && (url.charAt(i) == baseUrl.charAt(i))) {
          if (url.charAt(i) == '/') prefixLength = i
          i += 1
        }

        if (prefixLength > 0) {
          val sb = new StringBuilder()
          i = prefixLength+1; while (i < baseUrl.length) {
            if (baseUrl.charAt(i) == '/') sb.append("../")
            i += 1
          }
          val res = sb.append(url.substring(prefixLength+1)).result
          return if (res.endsWith("/.")) res.dropRight(2) else res
        }
      }

      // General case that can handle everything. Code above can be deleted and
      // everything will work just fine (albeit slowly).
      val link = new URI(url)
      val base = new URI(baseUrl)
      require(baseUrl.startsWith(blog.baseUrl) && base.isAbsolute)

      if (link.getHost != null && link.getHost != base.getHost) {
        url
      } else if (link.getPath == null) { // mailto: links
        url
      } else {
        val us = link.getPath.split("/").filter(_.nonEmpty)
        val bs = base.getPath.split("/").filter(_.nonEmpty)
        val prefixLen = (0 until us.length).prefixLength { i => us(i) == bs(i) }
        val backLevels = bs.length - prefixLen - 1
        val parts = Seq.fill(backLevels)("..") ++ us.drop(prefixLen)
        val partsWithoutLastDot = if (parts.length > 1 && parts.last == ".") parts.dropRight(1) else parts

        partsWithoutLastDot.mkString("/") +
          (if (link.getRawQuery != null) "?"+link.getRawQuery else "")+
          (if (link.getRawFragment != null) "#"+link.getRawFragment else "")
      }
    }

    def isLocalLink(url: String) = url.startsWith(blog.baseUrl+"/")
    def extractSlug(url: String) =
      if (isLocalLink(url)) Slug(url.drop(blog.baseUrl.length+1).dropRight(blog.fileSuffix.length))
      else sys.error("not local url: "+url)

    def addParamMediumFeed(url: String) =
      if (isAbsolute(url) && isLocalLink(url)) addParam(url, "utm_medium=feed") else url

    def thumbnailUrl(img: Image) = s"t/${img.thumb}-${blog.thumbWidth}x${blog.thumbHeight}"+imgSuffix(img)
    def bigThumbnailUrl(img: Image, half: Boolean) = s"t/${img.thumb}-${blog.bigThumbWidth / (if (half) 2 else 1)}"+imgSuffix(img)
  }



  val titleRegex        = """^(XXX+\s*)?(.+?)(?:\[([^ ]+)\])?$""".r
  private val dateRegex = """^(\d++)-(\d++)-(\d++)(?: (\d++):(\d++)(?::(\d++))?)?""".r
  val licenses = Set("CC by", "CC by-nc", "CC by-nd", "CC by-sa", "CC by-nc-nd", "CC by-nc-sa")

  def parseDates(l: String): Seq[Date] = {
    if (!l.charAt(0).isDigit) return null
    val dates = l.split(",").map(l => parseDate(l.trim))
    if (dates.nonEmpty && dates.forall(_ != null)) dates else null
  }

  private def parseDate(l: String): Date = {
    val matcher = dateRegex.pattern.matcher(l)
    if (!matcher.matches()) null else _parseDate(matcher)
  }

  private def parseDatePrefix(l: String): (Date, String) = {
    val matcher = dateRegex.pattern.matcher(l)
    if (!matcher.lookingAt()) (null, l) else (_parseDate(matcher), l.substring(matcher.end))
  }

  private def _parseDate(matcher: Matcher) = {
    def toInt(s: String) = if (s == null) 0 else s.toInt
    val y  = toInt(matcher.group(1))
    val m  = toInt(matcher.group(2))-1
    val d  = toInt(matcher.group(3))
    val h  = toInt(matcher.group(4))
    val mi = toInt(matcher.group(5))
    val s  = toInt(matcher.group(6))
    new GregorianCalendar(y, m, d, h, mi, s).getTime
  }

  def parseLicense(l: String): String =
    if (!licenses.contains(l)) null else l

  val tagBlockRegex = """(##|#)\s*(.+?)(?=( |^)(##|#)|$)""".r
  val tagSplitRegex = "\\s*,\\s*".r
  def isBracketed(x: String) = x.charAt(0) == '(' && x.charAt(x.length-1) == ')'
  def getTags(s: String): Tags =
    tagBlockRegex.findAllMatchIn(s).foldLeft(Tags()) { (t, m) =>
      val tags = tagSplitRegex.split(m.group(2))
      val (vis, hid) = (m.group(1) match {
        case "##" => (tags.map(Tag(_, true)), Array[Tag]())
        case "#"  =>
          val (v, h) = tags.partition(t => !isBracketed(t))
          (v.map(Tag(_)), h.map(t => Tag(t.substring(1, t.length-1))))
      })
      t.copy(visible = t.visible ++ vis, hidden = t.hidden ++ hid)
    }
  def getTags2(str: String) = {
    val visible = mutable.ArrayBuffer[Tag]()
    val hidden  = mutable.ArrayBuffer[Tag]()
    val s = Slurp(str)
    while (!s.isEnd) {
      s.chars('#', 1, 2).mustMatch()
      val supertag = s.matchLength > 1
      s.ignore()

      val sub = s.until('#').asView()
      while (!sub.isEnd) {
        sub.whitespaces().ignore()
        val t = sub.until(',').asString().trim
        sub.ignore(1)
        if (isBracketed(t)) {
          hidden += Tag(t.substring(1, t.length-1), supertag)
        } else {
          visible += Tag(t, supertag)
        }
      }
    }

    Tags(visible, hidden)
  }

  def peelOffTags(s: String): (String, Tags) = (tagBlockRegex.replaceAllIn(s, "").trim, getTags(s))
  def parseTags(l: String): Tags =
    if (l.charAt(0) != '#') null else getTags2(l)

  private def prefixedLine(prefix: String): (String => String) = { (l: String) =>
    if (!l.startsWith(prefix)) null else l.drop(prefix.length).trim
  }
  private def prefixedList(prefix: String): (String => Seq[String]) = { (l: String) =>
    if (!l.startsWith(prefix)) null else l.drop(prefix.length).split(",").map(_.trim)
  }

  def hash(txt: String): String = BigInt(1, _md5(txt.getBytes("utf-8"))).toString(16).reverse.padTo(32, '0').reverse
  def _md5(bytes: Array[Byte]) = MessageDigest.getInstance("MD5").digest(bytes)

  private val slugRegex = """[\w./+-]+""".r
  private val trailingWS = "\\s+$".r
  private def lastCharIsWhitespace(str: String) =
    str.length > 0 && Character.isWhitespace(str.charAt(str.length-1))

  def parseArticle(lines: Seq[String])(implicit blog: Blog): Article = {
    // 25 ms
    var ls = if (lines.exists(lastCharIsWhitespace)) lines.map(l => trailingWS.replaceAllIn(l, "")) else lines
    ls = ls.map{ l => if (l.startsWith("\\=")) l.tail else l }

    // 35 ms
    val titleRegex(xxx, dateTitle, slug) = ls(0)
    val (dateInTitle, title) = parseDatePrefix(dateTitle)

    // 40 ms
    val (metaLines, b) = ls.drop(2).span(l => l.nonEmpty)
    val body = b.slice(b.indexWhere(_.nonEmpty), b.lastIndexWhere(_.nonEmpty)+1)

    val _metaLines = metaLines.toArray

    // 190 ms
    val dates   = chompOne(_metaLines, parseDates, Seq.empty) /* 70 */
    val tags    = chompMany(_metaLines, parseTags).fold(Tags()){ _ merge _ } /* 41 */
    val license = chompOne(_metaLines, parseLicense)
    val links   = chompOne(_metaLines, prefixedLine("link:"))
    val notess  = chompOne(_metaLines, prefixedLine("notes:"))
    val authors = chompOne(_metaLines, prefixedLine("by:"), blog.defaultUser)
    val meta    = Meta(chompOne(_metaLines, prefixedList("meta:"), Seq.empty)) /* 27 */
    val rels    = chompOne(_metaLines, prefixedList("rel:"), Seq.empty)
    val pubs    = chompOne(_metaLines, prefixedList("pub:"), Seq.empty)
    val aliass  = chompOne(_metaLines, prefixedList("alias:"), Seq.empty)
    val implies = Option(chompOne(_metaLines, prefixedLine("implies:"))).map(x => parseTags(x).visible).getOrElse(Seq.empty)
    val imgtags = Option(chompOne(_metaLines, prefixedLine("imageTags:"))).map(x => parseTags(x).visible).getOrElse(Seq.empty)

    // /--- 23ms
    if (unchompedLines(_metaLines) > 0)
      sys.error("some metainformation was not processed: "+_metaLines.toSeq)

    val isTag = meta.contains("supertag") || meta.contains("tag") || (slug != null && isTagSlug(slug))
    val inFeed = blog.dumpAll || (xxx == null && !isTag)
    val realSlug =
      if (slug != null && slug != "") slug else
        if (isTag) tagSlug(title)
        else generateSlug(title)

    if ((slug == null || slug == "") && blog.demandExplicitSlugs && !title.startsWith("???"))
      sys.error(s"article ${title} is missing explicit slug")

    if (slug != null && slug.nonEmpty && !slugRegex.pattern.matcher(slug).matches())
      sys.error(s"slug '$slug' is not valid, only letters, numbers and ./+- allowed")

    if (links != null)
      require(isAbsolute(links), s"urls in link: field must be absolute ($realSlug)")
    // \---

    new Article(
      title   = if (title.trim.nonEmpty) title.trim else dateTitle.trim,
      slug    = realSlug,
      author  = authors,
      dates   = if (dateInTitle != null) dateInTitle +: dates else dates,
      tags    = tags,
      meta    = meta,
      rel     = rels,
      pub     = pubs,
      aliases = aliass,
      implies = implies,
      imgtags = imgtags,
      link    = links,
      notes   = notess,
      license = license,
      rawText = body,
      inFeed  = inFeed
    )
  }

  def chompOne[T](ms: Array[String], f: String => T, default: T = null.asInstanceOf[T]): T = {
    var i = 0; while (i < ms.length) {
      if (ms(i) != null) {
        val res = f(ms(i))
        if (res != null) {
          ms(i) = null
          return res
        }
      }
      i += 1
    }
    default
  }
  def chompMany[T](ms: Array[String], f: String => T): Seq[T] = {
    val b = Seq.newBuilder[T]
    var i = 0; while (i < ms.length) {
      if (ms(i) != null) {
        val res = f(ms(i))
        if (res != null) {
          ms(i) = null
          b += res
        }
      }
      i += 1
    }
    b.result
  }
  def unchompedLines(ms: Array[String]) = ms.count(_ != null)


  private val _multipleDashes = "--+".r
  private val _trailingDashes = "-+$|^-+".r
  private val slugFrom = "áčďéěíňóřšťúůýž"
  private val slugTo   = "acdeeinorstuuyz"
  private val slugTxl: Map[Char, Char] = (slugFrom zip slugTo).toMap withDefault (x => x)

  def generateSlug(title: String): String = {
    var t = title.toLowerCase
      .map(slugTxl)
      .map(ch => if (Character.isAlphabetic(ch) || Character.isDigit(ch)) ch else '-')

    t = _multipleDashes.replaceAllIn(t, "-")
    _trailingDashes.replaceAllIn(t, "")
  }

  def tagSlug(tag: String) = "tag/"+generateSlug(tag)
  def isTagSlug(tag: String) = tag.startsWith("tag/")




  private def calendar(d: Date, field: Int) = {
    val cal = new GregorianCalendar()
    cal.setTime(d)
    cal.get(field)
  }

  def year(d: Date): Int = calendar(d, Calendar.YEAR)
  def month(d: Date): Int = calendar(d, Calendar.MONTH)+1
  def yearmonth(d: Date) = (year(d), month(d))


  private val rssDateFormat = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = new SimpleDateFormat("EEE', 'dd' 'MMM' 'yyyy' 'HH:mm:ss' 'Z", Locale.US)
  }

  def rssdate(date: Date) = if (date == null) "" else
    rssDateFormat.get.format(date)

  def makeRSS(articles: Seq[Article], mkBody: Article => String)(implicit blog: Blog): String = {
    XMLSW.Simple.document { w =>
      w.element("rss", Seq("version" -> "2.0")) { w =>
        w.element("channel") { w =>
          w.element("title", blog.title)
          for (a <- articles) {
            w.element("item") { w =>
              w.element("title", a.title)
              w.element("guid", Seq(("isPermaLink", "true")), blog.addParamMediumFeed(blog.absUrlFromSlug(a.slug)))
              w.element("pubDate", rssdate(a.date))
              if (mkBody != null) {
                w.element("description", mkBody(a))
              }
            }
          }
        }
      }
    }.toString
  }



  def saveFile(f: String, content: String, fileIndex: Map[String, String] = Map())(implicit blog: Blog): Seq[(String, String)] = { // filename -> hash
    val ff = new File(blog.outDir, f)

    val p = ff.getParentFile
    if (p != null) p.mkdirs()

    val h = hash(content)

    if (!fileIndex.contains(f) || fileIndex(f) != h || !ff.exists) {
      val fw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(ff), "utf-8"))
      fw.write(content)
      fw.close()
    }

    Seq(f -> h)
  }

  def saveXml(f: String, content: String, fileIndex: Map[String, String] = Map())(implicit blog: Blog): Seq[(String, String)] =
    saveFile(f+".xml", content, fileIndex)



  def readGallery(blog: Blog): Vector[Article] = {
    if (blog.albumsDir.isEmpty) return Vector()

    val albumDirs = new File(blog.albumsDir, "albums").listFiles.sortBy(_.getName).reverse.toSeq
    val dateTitle = """^(?:(\d+)-(\d+)-(\d+)\s*-?\s*)?(.*)$""".r

    for (albumDir <- albumDirs.toVector) yield {
      println(albumDir.getName)

      val dateTitle(y, m, d, t) = albumDir.getName
      val (date, title) =
        if (y == null) (null, t)
        else (new GregorianCalendar(y.toInt, m.toInt-1, d.toInt).getTime, t)

      def validSuffix(f: String) = {
        val ff = f.toLowerCase
        ff.endsWith(".jpg") || ff.endsWith(".png") || ff.endsWith(".gif")
      }

      val imgUrls = albumDir.list collect { case f if validSuffix(f) =>
        blog.baseUrl + new URI(null, null, "/albums/"+albumDir.getName+"/"+f, null).toASCIIString
      }

      new Article(
        title  = if (title.nonEmpty) title else albumDir.getName,
        slug   = generateSlug(albumDir.getName),
        dates  = Seq(date),
        images = imgUrls.map { url => new Image(url, inText = false) }
      )
    }
  }

  def readPosts(implicit blog: Blog): Vector[Article] = {
    val lineRegex = """^===+$""".r.pattern
    blog.files.iterator.flatMap { f =>
      var ls = io.Source.fromFile(f, 1024*128).getLines.toArray
      val starts = (0 until ls.length).collect { case i if ls(i).startsWith("===") && lineRegex.matcher(ls(i)).matches() => i-1 }
      (0 until starts.length).map { i =>
        parseArticle(ls.slice(starts(i), starts.lift(i+1).getOrElse(ls.length)))
      }
    }.toVector
  }






  def makeBase(implicit blog: Blog, markup: Markup): (Blog, Base, String => String) = {
    var articles: Vector[Article] = timer("readfiles")(readGallery(blog) ++ readPosts(blog))

    timer("checks") {
    if (blog.articlesMustNotBeMixed) {
      val (hidden, rest1) = articles.span { a => a.title.startsWith("?") }
      val (visible, rest) = rest1.span { a => !a.title.startsWith("?") }
      if (rest.nonEmpty) sys.error("hidden and visible articles are mixed up")
    }

    // this is done ahead of time beucase of article merging
    val hiddenSlugs: Set[String] = articles.iterator.collect { case a if  a.title.startsWith("?") => a.slug }.toSet

    for (a <- articles if !a.title.startsWith("?")) {
      if (a.dates.nonEmpty && hiddenSlugs.contains(a.slug)) // dated article has hidden counterpart
        sys.error(s"hidden and dated article are sharing the same slug '${a.slug}', this is most likely an error")
    }

    val now = new Date
    articles = articles.filter { a =>
      val isInPast = a.date == null || a.date.before(now)
      blog.dumpAll || (!hiddenSlugs.contains(a.slug) && isInPast)
    }
    }

    timer("merge") {
    // slug dupes and article merging
    val articleMap = mutable.Map[String, Article]()
    val slugOrder  = mutable.ArrayBuffer[String]()

    for (a <- articles) {
      if (!articleMap.contains(a.slug)) {
        articleMap.update(a.slug, a)
        slugOrder += a.slug

      } else {
        val b = articleMap(a.slug)

        if (
          (a.dates.nonEmpty && b.dates.nonEmpty) ||
          (a.tags != Tags() && b.tags != Tags()) ||
          (a.author != b.author && a.author != null && b.author != null) ||
          (a.license != null && b.license != null) ||
          (a.meta != Meta() && b.meta != Meta()) //||
        ) sys.error("two conflicting articles with the same slug '"+a.slug+"'")

        val merged = b.copy(
          dates   = a.dates ++ b.dates,
          tags    = a.tags merge b.tags,
          meta    = a.meta merge b.meta,
          rel     = a.rel ++ b.rel,
          pub     = a.pub ++ b.pub,
          aliases = a.aliases ++ b.aliases,
          license = if (a.license != null) a.license else b.license,
          rawText = {
            val (short, long) = if (a.rawText.size < b.rawText.size) (a.rawText, b.rawText) else (b.rawText, a.rawText)
            if (short.isEmpty) long else short ++ Seq("", "") ++ long
          },
          images  = a.images ++ b.images,
          inFeed  = a.inFeed && b.inFeed
        ) // backlinks, similar, pubArticles, pubBy not yet populated

        articleMap(a.slug) = merged
      }
    }

    articles = slugOrder.map(articleMap).toVector
    }

    if (blog.articlesMustBeSorted) { // ordered by date
    timer("order by date") {
      val dated = articles.filter(_.date != null)
      val ordered = dated == dated.sortBy(~_.date.getTime)
      if (!ordered) sys.error("articles are not ordered by date")
    }
    }

    timer("tagImplications") {
    val tagImplications: Map[Tag, Seq[Tag]] =
      (for (a <- articles if a.isTag && a.implies.nonEmpty) yield (a.asTag, a.implies)).toMap

    articles = articles.map { a =>
      if (a.tags.visible.forall(t => !tagImplications.contains(t))) { // nothing to imply
        a
      } else {
        val implied = a.tags.visible.flatMap { t => tagImplications.getOrElse(t, Seq()) }
        a.copy(tags = a.tags.copy(visible = (a.tags.visible ++ implied).distinct))
      }
    }
    }

    // Maps all article slugs (main ones and aliases) to their absolute urls.
    // Bit of a hack, this lazy function closes over mutable variable articles.
    // So when it's finally called it sould use the most recent and up to date
    // collection of articles. This is needed to resolve circular dependency:
    // global names -> parse text -> materializeNonexplicitTags -> global names
    lazy val globalNames: Map[String, String] = timer("global names") {
      articles.iterator.flatMap { a => (a.slug +: a.aliases).map(s => (s, blog.absUrl(a))) }.toMap
    }

    val fileSuffixes = ".*\\.(php|jpg|png|gif|rss|zip|data|txt|scala|c)$".r.pattern

    // globalMapping maps from slugs to absolute urls
    def resolveLink(link: String, globalMapping: Map[String, String], a: Article = null): String = {
      val (b, h) = util.splitByHash(link)
      if (b == "index") {
        blog.baseUrl+"/."
      } else if (b.isEmpty || b.startsWith("..") || b.contains(".") || fileSuffixes.matcher(b).matches() || isAbsolute(b)) {
        link
      } else if (!globalMapping.contains(b)) {
        println(s"bad link [$link] (in ${if (a == null) null else a.slug})")
        Blog.invalidLinkMarker
      } else {
        globalMapping(b)+h
      }
    }

    val resolver = (link: String) => resolveLink(link, globalNames, null)

    timer("parse text") {
    articles = articles.map { a =>
      val noteUrl = if (a.notes == null) "" else blog.absUrlFromSlug(a.notes)
      val txt = markup.process(a.rawText, link => resolveLink(link, globalNames, a), noteUrl, blog.imageRoot)
      a.copy(
        text = txt,
        images = (a.images ++ txt.images) // images might be already populated from readGallery()
      )
    }
    }

    def materializeNonexplicitTags(all: Vector[Article]): Vector[Article] = timer("materializeNonexplicitTags") {
      val explicitTags: Set[Tag] = all.collect { case a if a.isTag => a.asTag }.toSet
      val mentionedTags: Set[Tag] = all.flatMap { a => a.tags.visible ++ a.tags.hidden ++ a.implies ++ a.imgtags ++ a.images.flatMap(i => i.tags.visible ++ i.tags.hidden) }.toSet
      (mentionedTags -- explicitTags).map { t =>
        Article(t.title, tagSlug(t.title), meta = Meta(Seq(if (t.supertag) "supertag" else "tag")), text = AsciiText.empty)
      }.toVector
    }

    articles = articles ++ materializeNonexplicitTags(articles)

    articles = articles.map { a =>
      if (a.imgtags.isEmpty) a else a.addImageTags(a.imgtags)
    }

    timer("checks") {
    val allIds = new mutable.HashSet[String]()
    for (a <- articles.iterator.map(_.slug) ++ articles.iterator.flatMap(_.aliases)) {
      if (allIds.contains(a)) { sys.error(s"duplicate slug/alias '${a}'") }
      allIds += a
    }

    articles foreach { a =>
      a.pub foreach { id => if (!allIds(id)) sys.error(s"id [$id] is not defined (used as pub in article '${a.title})'") }
      a.rel foreach { id => if (!allIds(id)) sys.error(s"id [$id] is not defined (used as rel in article '${a.title})'") }
    }
    }

    timer("canonize pubs and rels") {
    val canonicalSlugs: Map[String, String] = // [alias -> canonical slug]
      invert1(articles.collect { case a if a.aliases.nonEmpty => (a.slug, a.aliases) })

    articles = articles.map { a =>
      a.copy(
        pub = a.pub.map(id => canonicalSlugs.getOrElse(id, id)),
        rel = a.rel.map(id => canonicalSlugs.getOrElse(id, id))
      )
    }
    }

    val backlinks: Map[Slug, Seq[Article]] = timer("backlinks") {
      invert(articles.map { a => (a, a.slugsOfLinkedArticles) })
        .map { case (k, as) => (k, as.distinct) }
    }

    val pubsBy: Map[Slug, Article] = timer("pubsBy") {
      invert(articles.map { a => (a, a.pub.map(Slug)) })
        .map { case (k, vs) => (k, vs.minBy(_.date)) }
    }

    val byDate = (a: Article) => ~(if (a.date == null) 0 else a.date.getTime)

    articles = timer("populate") {
      val sim = new Similarities(articles)
      val base = Base(articles, null)

      articles.map { a =>
        val bs = backlinks.getOrElse(a.asSlug, Seq())
        val pubBy = pubsBy.getOrElse(a.asSlug, null)

        a.copy(
          dates = if (a.dates.isEmpty && pubBy != null) pubBy.dates.take(1) else a.dates,
          backlinks = bs.sortBy(byDate), //sim.sortBySimilarity(bs, a),
          similar = if (!a.isTag) sim.similarByTags(a, count = blog.limitSimilar, without = bs) else sim.similarTags(a.asTag, count = blog.limitSimilar),
          pubArticles = a.pub.map(base.bySlug),
          pubBy = pubBy
        )
      }
    }

    var tagMap = invert(articles.map { a => (a, (a.tags.visible).distinct) })

    if (blog.sortByDate) { // newest first, articles without date last
      articles = articles.sortBy(byDate)
      tagMap = tagMap.map { case (t, as) => (t, as.sortBy(byDate)) }
    }

    tagMap = timer("final tagmap") {
      val tagArticle = articles.iterator.collect { case a if a.isTag => (a.asTag, a) }.toMap

      tagMap.map { case (t, as) =>
        val key = tagArticle(t).meta.value("sortby")

        if (key == null) { // sort by order linked in article (order for << >> navigation)
          val linked = tagArticle(t).slugsOfLinkedArticles.distinct
          val tagged = as.map(_.asSlug)
          val artMap = as.map(a => (a.asSlug, a)).toMap
          (t, ((linked intersect tagged) ++ (tagged diff linked)).map(artMap))

        } else if (key == "title") {
          (t, as.sortBy(_.title))

        } else {
          (t, as.sortBy { a =>
            val k = a.meta.value(key)
            if (k == null) 0 else ~k.toInt
          })
        }
      }
    }

    (blog, Base(articles, tagMap), resolver)
  }




  def makeFiles(blog: Blog, base: Base, markup: Markup, resolver: String => String) = try {
    implicit val _blog = blog

    val oldFileIndex: Map[String, String] = {
      val f = new File(blog.outDir, ".files")
      if (!f.exists) Map()
      else io.Source.fromFile(f).getLines.map{ l => val Array(k, v) = l.split(" ", 2); (v, k) }.toMap
    }


    val fileIndex = collection.concurrent.TrieMap[String, String]()
    val isIndexGallery = base.feed.take(blog.articlesOnIndex).exists(_.images.nonEmpty)

    val (fulls, rest) = base.feed.splitAt(blog.articlesOnIndex)

    def chunk[T: Ordering](as: Vector[Article])(g: Date => T)(zero: T)(f: T => Article): Vector[(Article, Vector[Article])] =
      as.groupBy { a => if (a.date == null) zero else g(a.date) }.toVector.sortBy(_._1).reverse.map { case (t, as) => (f(t), as) }

    val (links, archivePages) = timer("group archive") {
      def mkDate(y: Int, m: Int) = if (y == 0) Seq() else Seq(new GregorianCalendar(y, m-1, 1).getTime)
      val title = blog.translation("archive")
      (blog.groupArchiveBy match {
        case "month" => chunk(rest)(yearmonth)((0,0)) { case (y, m) => Article(title+s" $m/$y", s"index-$y-$m", dates = mkDate(y, m)) }
        case "year"  => chunk(rest)(year)     (0)     { case y      => Article(title+s" $y", s"index-$y", dates = mkDate(y, 1)) }
        case num if num.trim.matches("\\d+") =>
          val len = num.toInt
          rest.reverse.grouped(len).toVector.zipWithIndex.map { case (as, i) =>
            (Article(title+s" #${i*len+1}-${(i+1)*len}", "index-"+(i+1)), as.reverse)
          }.reverse
        case _ => Vector((null, rest))
      }) match {
        case (_, links) +: archivePages => (links, archivePages)
        case _ => (Vector(), Vector())
      }
    }

    val layout = new FlowLayoutMill(base, blog, markup, resolver)

    timer("generate and save files") {
    timer("generate and save files - archive") {
    val archiveLinks = archivePages.zipWithIndex.map { case ((a, as), idx) =>
      val l = layout.make(blog.absUrl(a))
      val prev = archivePages.lift(idx-1).map(_._1).getOrElse(null)
      val next = archivePages.lift(idx+1).map(_._1).getOrElse(null)
      val body = l.addArrows(l.makeIndex(Seq(), as), prev, next, true)
      fileIndex ++= saveFile(blog.relUrl(a), l.makePage(body, containImages = as.exists(_.hasImageMarker)), oldFileIndex)
      a
    }

    val path = blog.relUrlFromSlug("index")
    val l = layout.make(blog.absUrlFromPath(path))
    val body = l.makeIndex(fulls, links, archiveLinks, blog.groupArchiveBy == "month")
    fileIndex ++= saveFile(path, l.makePage(body, containImages = isIndexGallery), oldFileIndex)
    }

    timer("generate and save files - image pages") {
    val groupedImages = base.images.reverse.grouped(100).toVector.zipWithIndex.reverse
    val imgsPages = groupedImages.map { case (images, idx) =>
      val isFirst = idx == (groupedImages.size-1)
      Article(s"imgs ${idx+1}", if (isFirst) "imgs" else s"imgs-${idx+1}", text = AsciiText.empty, images = images)
    }

    imgsPages.zipWithIndex foreach { case (a, idx) =>
      var l = layout.make(blog.absUrl(a))
      val prev = imgsPages.lift(idx-1).getOrElse(null)
      val next = imgsPages.lift(idx+1).getOrElse(null)
      val body = l.addArrows(l.makeFullArticle(a), prev, next, true)
      fileIndex ++= saveFile(blog.relUrl(a), l.makePage(body, a.title, containImages = true), oldFileIndex)
    }
    }

    timer("generate and save files - articles") {
    base.articles.par foreach { a =>
      var l = layout.make(blog.absUrl(a))
      val body = l.makeFullArticle(a.imagesWithoutArticleTags)
      fileIndex ++= saveFile(blog.relUrl(a), l.makePage(body, a.title, containImages = a.images.nonEmpty, headers = l.ogTags(a)), oldFileIndex)
    }
    }

    base.allTags foreach { case (t, (a, as)) =>
      var l = layout.make(blog.absUrl(a))
      val body = l.makeFullArticle(a.imagesWithoutArticleTags)
      val hasImages = a.images.nonEmpty || as.exists(_.images.nonEmpty)
      fileIndex ++= saveFile(blog.relUrl(a), l.makePage(body, a.title, containImages = hasImages, headers = l.rssLink(a.slug+".xml")), oldFileIndex)
      fileIndex ++= saveXml(a.slug, makeRSS(as.take(blog.limitRss), null), oldFileIndex)
    }

    {
      val path = blog.relUrlFromSlug("tags")
      val l = layout.make(blog.absUrlFromPath(path))
      fileIndex ++= saveFile(path, l.makePage(l.makeTagIndex(base)), oldFileIndex)
    }

    def mkBody(a: Article) = {
      val body = layout.make(null).makeArticleBody(a, true)
      FlowLayout.updateLinks(body, url => blog.addParamMediumFeed(url))
    }
    fileIndex ++= saveXml("rss", makeRSS(base.feed.take(blog.limitRss), if (blog.articlesInRss) mkBody else null), oldFileIndex)

    if (blog.allowComments) {
      val l = layout.make(blog.absUrlFromPath("comments.php"))
      val p = l.makePage("{comments.body}", null, false,  null, includeCompleteStyle = true)
      val Array(pre, post) = p.split(Regex.quote("{comments.body}"))

      fileIndex ++= saveFile("comments.php", {
        val replaces = blog.translation.collect { case (k, v) if k.startsWith("comments.") => s"{$k}" -> v } ++ Seq(
          "{comments.prebody}"  -> pre,
          "{comments.postbody}" -> post,
          "{comments.baseUrl}"  -> blog.baseUrl,
          """href="rss.xml""""  -> """href="'.escapeHtmlAttr($requestUrl).'&amp;rss"""" // this is a bit ugly trick
        )
        var cs = commentsScript
        for ((from, to) <- replaces) {
          cs = cs.replace(from, to)
        }
        cs
      }, oldFileIndex)
      new File(".comments").mkdirs()
      fileIndex ++= saveFile(".comments/.htaccess", "Deny from all", oldFileIndex)
    }

    //fileIndex ++= saveFile("out.php", outScript, oldFileIndex)

    if (blog.cssFile.nonEmpty) {
      fileIndex ++= saveFile("style.css", io.Source.fromFile(blog.cssFile).mkString, oldFileIndex)
    }

    fileIndex ++= saveFile("robots.txt", "User-agent: *\nAllow: /", oldFileIndex)
    saveFile(".files", fileIndex.toSeq.sorted.map { case (file, hash) => hash+" "+file }.mkString("\n"), oldFileIndex)
    }


    timer("resize images") {
    def smallThumbJob(img: Image) = (new File(blog.outDir, blog.thumbnailUrl(img)),           blog.thumbWidth,      blog.thumbHeight, 0.05f)
    def mainThumbJob(img: Image)  = (new File(blog.outDir, blog.bigThumbnailUrl(img, false)), blog.bigThumbWidth,   -1,               0.1f)
    def halfThumbJob(img: Image)  = (new File(blog.outDir, blog.bigThumbnailUrl(img, true)),  blog.bigThumbWidth/2, -1,               0.1f)

    // always make small thumbnails
    // make big thumbnails for the first image in article
    val resizeJobs = base.all.flatMap { a =>
      def first = a.images.head
      a.images.map {
        case i if i == first                         => (i, Seq(smallThumbJob(i), mainThumbJob(i), halfThumbJob(i)))
        case i if i.mods == "main" && i.align == ">" => (i, Seq(smallThumbJob(i), halfThumbJob(i)))
        case i if i.mods == "main"                   => (i, Seq(smallThumbJob(i), mainThumbJob(i)))
        case i                                       => (i, Seq(smallThumbJob(i)))
      }
    }

    new File(blog.outDir, "t").mkdir()

    import java.awt.image. { BufferedImage }
    import javax.imageio. { IIOImage, ImageWriteParam }

    def saveJpg(thumbFile: File, img: BufferedImage, quality: Float) = {
      val ios = ImageIO.createImageOutputStream(thumbFile)
      val writer = ImageIO.getImageWritersByFormatName("jpeg").next()
      val params = writer.getDefaultWriteParam()
      params.setCompressionMode(ImageWriteParam.MODE_EXPLICIT)
      params.setCompressionQuality(quality)
      writer.setOutput(ios)
      writer.write(null, new IIOImage(img, null, null), params)
      writer.dispose()
    }

    for ((image, jobs) <- resizeJobs) {
      try {
        lazy val file = {
          println(s"downloading ${image.url}")
          ImageIO.read(new URL(image.url))
        }
        for ((thumbFile, w, h, sharpenStrength) <- jobs if !thumbFile.exists) {
          println(s"resizing image ${image.url} -> $thumbFile")
          file match {
            case null => println(s"ImageIO.read(${image.url}) == null")
            case full =>
              val suffix = image.url.split("\\.").last.toLowerCase
              val s = if (ImageIO.getWriterFileSuffixes.contains(suffix)) suffix else "jpg"
              val leeway = if (s == "png" || s == "gif") 1.6 else 1.6
              val strength = if (s == "png" || s == "gif") 0f else sharpenStrength
              val resized = ImageTools.resizeImage(full, w, h, leeway, strength)
              if (s == "jpg" || s == "jpeg") {
                saveJpg(thumbFile, resized, 0.85f)
              } else {
                ImageIO.write(resized, s, thumbFile)
              }
          }
        }
      } catch { case e: IIOException => println(e) }
    }
    }


  println(T.t)

  } catch {
    case e: Exception =>
      // If anything goes wrong nuke .files index.
      // It's necessary for correct conditional save in `saveFile` method.
      // In cases when I make some changes, program may overwrite few files and
      // than explode without updating .files index. Then if I revert that
      // change, program incorrectly thinks (according to index) that all files
      // are unchanged and it not save correct version. Deleting .files index
      // prevents this problem, because after every error, all files are
      // regenerated.
     new File(blog.outDir, ".files").delete()
     throw new Exception(e)
  }
}
