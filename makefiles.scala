package asciiblog

import MakeFiles.{ hash, UrlOps, isAbsolute }
import java.io.{ File, BufferedWriter, OutputStreamWriter, FileOutputStream, FileInputStream }
import java.net.{ URL, URI, URLDecoder, URLEncoder }
import java.security.MessageDigest
import java.text.SimpleDateFormat
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.regex.Matcher
import java.awt.Color
import javax.imageio.{ ImageIO, IIOException }
import scala.collection.mutable
import scala.util.matching.Regex
import util._

object T { val t = new Timer; def apply[T](f: => T) = t.apply(f) }

object Make extends App {
  val timer = new Timer()
  timer.start()

  if (args.length < 1) {
    println("config file not specified")
    sys.exit()
  }

  try {
    val (blog, base, resolver, changed) = MakeFiles.init(args)
    MakeFiles.makeFiles(blog, base, resolver, changed)

    timer.end()
    println("total: "+timer.ms)

    blog.hooks.afterGenerate(base, blog)
  } catch {
    case e: ConfigurationException =>
      println("configuration error: "+e.message)
  }
}


class ConfigurationException(val message: String, val valueGiven: String = null) extends Exception(message)


case class Blog (
  val title: String,
  val baseUrl: String,
  val imageRoot: String,
  val files: Seq[File],
  val imageDir: File,
  val localImages: Boolean,
  val encoding: String,
  val outDir: File,
  val groupArchiveBy: String,
  val indexCfg: Seq[String],
  val archiveCfg: Seq[String],
  val tagsCfg: Seq[String],
  val rssCfg: Seq[String],
  val rssTagsCfg: Seq[String],
  val cssStyle: String,
  val cssExport: Boolean,
  val cssFile: File,
  val galleryScriptExport: Boolean,
  val header: String,
  val footer: String,
  val thumbWidth: Int,
  val thumbHeight: Int,
  val bigThumbWidth: Int,
  val similarLimit: Int,
  val sortByDate: Boolean,
  val articlesMustBeSorted: Boolean,
  val articlesMustNotBeMixed: Boolean,
  val language: String,
  val fileSuffix: String,
  val imageMarker: String,
  val albumsDir: String,
  val allowComments: Boolean,
  val commentsModeration: Boolean,
  val allowShareScript: Boolean,
  val shareLinks: Boolean,
  val demandExplicitSlugs: Boolean,
  val excludeFutureArticles: Boolean,
  val usersAsTags: Boolean,

  val defaultUser: String,
  val openGraph: Boolean,
  val twitterSite: String,
  val twitterCreator: String,
  val textCards: Boolean,
  val textCardsBackground: Color,
  val textCardsColor: Color,

  val args: Array[String],
  val translation: Map[String, String],
  val hooks: Hooks = null,
  val markup: Markup = null,

  val printTiming: Boolean  = false,
  val printErrors: Boolean = true,
  val trackChangedArticles: Boolean = false,

  val cfg: Map[String, String] = Map()
) extends UrlOps {
  def hasOgTags = twitterSite.nonEmpty || twitterCreator.nonEmpty || openGraph
  def quiet = copy(printErrors = false, printTiming = false)
}



object Blog {
  def populate(cfg: Map[String, String], args: Array[String], translation: Map[String, String], cfgFile: File) = {
    val cfgDirectory = cfgFile.getParentFile
    val inline = if (cfg.contains("inline!")) Seq(cfgFile) else Seq()

    def cfgStr_! (key: String): String = {
      if (!cfg.contains(key)) throw new ConfigurationException(s"key `$key` required")
      cfg(key).trim
    }
    def cfgStr (key: String, default: String) = cfg.getOrElse(key, default).trim
    def cfgBool(key: String, default: Boolean) =
      cfgStr(key, default.toString).toLowerCase match {
        case "true"  | "on"  | "yes" | "1" => true
        case "false" | "off" | "no"  | "0" => false
        case str => throw new ConfigurationException(s"$key: expected boolean, '$str' given")
      }
    def cfgInt (key: String, default: Int) = {
      val str = cfgStr(key, default.toString)
      try str.toInt catch {
        case e: java.lang.IllegalArgumentException =>
          throw new ConfigurationException(s"$key: expected integer, '$str' given")
      }
    }
    def cfgColor(key: String, default: Color): Color = {
      if (!cfg.contains(key)) return default
      val value = cfg(key).trim
      if (value.isEmpty) return default

      def hex1(s: String) = Integer.parseInt(s, 16)*17
      def hex2(s: String) = Integer.parseInt(s, 16)

      if (value.startsWith("#")) {
        val Seq(r, g, b, a) = value.length match {
          case 4 => value.drop(1).grouped(1).map(hex1).toSeq :+ 255
          case 5 => value.drop(1).grouped(1).map(hex1).toSeq
          case 7 => value.drop(1).grouped(2).map(hex2).toSeq :+ 255
          case 9 => value.drop(1).grouped(2).map(hex2).toSeq
          case _ => throw new ConfigurationException(s"$key: invalid color '$value'")
        }
        new Color(r, g, b, a)

      } else {
        try {
          classOf[Color].getField(value).get(null).asInstanceOf[Color]
        } catch {
          case _: NoSuchFieldException | _ :ClassCastException =>
            throw new ConfigurationException(s"$key: invalid color '$value'")
        }
      }
    }

    def check[T](x: T)(f: T => Boolean)(message: T => String) = { require(f(x), message(x)); x }
    def alts(str: String, alternatives: Map[String, String]) = alternatives.getOrElse(str.toLowerCase, str)
    def initClass[T](line: String): T = {
      val Array(clazz, args @ _*) = line.split(" ")
      Class.forName(clazz)
        .getDeclaredConstructor(args.map(_.getClass): _*)
        .newInstance(args: _*).asInstanceOf[T],
    }

    val hooks                  = initClass[Hooks](cfgStr("hooks", "asciiblog.NoHooks"))
    val markup                 = initClass[Markup](alts(cfgStr("markup", "asciiblog.AsciiMarkup"), Map(
        "html" -> "asciiblog.HTMLMarkup", "ascii" -> "asciiblog.AsciiMarkup")))


    val b = new Blog(
      title                  = cfgStr_!("title"),
      baseUrl                = cfgStr_!("baseUrl"),
      imageRoot              = cfgStr ("imageRoot", ""),
      files                  = inline ++ spaceSeparatedStrings(cfgStr("files", "")).flatMap(f => globFiles(f, cfgDirectory)), // relative paths are relative to config file
      imageDir               = cfg.get("imageDir").fold(cfgDirectory)(f => if (f.isEmpty) cfgDirectory else newFile(f.trim, cfgDirectory)),
      localImages            = cfgBool("localImages", true),
      encoding               = cfgStr ("encoding", "utf-8"),
      outDir                 = cfg.get("outDir").map(f => newFile(f.trim, cfgDirectory)).getOrElse(null),

      groupArchiveBy         = check(cfgStr("groupArchiveBy", "year"))
        (f => f == "year" || f == "month" || f == "none" || f.matches("\\d+"))
        (f => s"groupArchiveBy must be set to 'year', 'month', 'none' or some integer, '$f' given"),

      indexCfg               = cfgStr("index",   "full 5\narchive").linesIterator.map(_.trim).toVector,
      archiveCfg             = cfgStr("archive", "link").linesIterator.map(_.trim).toVector,
      tagsCfg                = cfgStr("tags",    "link").linesIterator.map(_.trim).toVector,
      rssCfg                 = cfgStr("rss",     "link").linesIterator.map(_.trim).toVector,
      rssTagsCfg             = cfgStr("rssTags", "link").linesIterator.map(_.trim).toVector,

      cssStyle               = cfgStr ("style", ""),
      cssExport              = cfgBool("cssExport", false),
      cssFile                = cfg.get("cssFile").map(f => newFile(f.trim, cfgDirectory)).getOrElse(null),
      galleryScriptExport    = cfgBool("galleryScriptExport", false),
      header                 = cfgStr ("header", ""),
      footer                 = cfgStr ("footer", ""),
      thumbWidth             = cfgInt ("thumbnailWidth", 150),
      thumbHeight            = cfgInt ("thumbnailHeight", 100),
      bigThumbWidth          = cfgInt ("bigThumbnailWidth", 800),
      similarLimit           = cfgInt ("similarLinksLimit", 5),
      sortByDate             = cfgBool("sortByDate", false),
      articlesMustBeSorted   = cfgBool("articlesMustBeSorted", false),
      articlesMustNotBeMixed = cfgBool("articlesMustNotBeMixed", false),
      language               = cfgStr ("language", "en").trim,
      fileSuffix             = cfgStr ("fileSuffix", ".html"),
      imageMarker            = cfgStr ("imageMarker", ""),
      albumsDir              = cfgStr ("albumsDir", ""),
      allowComments          = cfgBool("allowComments", false),
      commentsModeration     = cfgBool("commentsModeration", false),
      allowShareScript       = cfgBool("allowShareScript", false),
      shareLinks             = cfgBool("shareLinks", false),
      demandExplicitSlugs    = cfgBool("demandExplicitSlugs", false),
      excludeFutureArticles  = cfgBool("excludeFutureArticles", false),
      usersAsTags            = cfgBool("usersAsTags", false),

      defaultUser            = cfgStr ("defaultUser", ""),
      openGraph              = cfgBool("openGraph", false),
      twitterSite            = cfgStr ("twitter.site", ""),
      twitterCreator         = cfgStr ("twitter.creator", ""),
      textCards              = cfgBool("textCards", false),
      textCardsBackground    = cfgColor("textCards.background", Color.WHITE),
      textCardsColor         = cfgColor("textCards.color", Color.BLACK),

      args                   = args,
      translation            = translation ++ cfg.collect { case (k, v) if k.startsWith("translation.") => k.split("\\.", 2)(1) -> v } ,

      hooks                  = hooks,
      markup                 = markup,

      printTiming            = cfgBool("printTiming", false),
      printErrors            = cfgBool("printErrors", true),
      trackChangedArticles   = cfgBool("trackChangedArticles", false),

      cfg = cfg
    )

    if (!isAbsolute(b.baseUrl)) {
      throw new ConfigurationException(s"baseUrl must be aboslute url, '${b.baseUrl}' given")
    }

    if (b.imageRoot.nonEmpty && !isAbsolute(b.imageRoot)) {
      throw new ConfigurationException(s"imageRoot must be aboslute url, '${b.imageRoot}' given")
    }

    val imgRoot1 = if (b.imageRoot.nonEmpty) b.imageRoot else b.baseUrl
    val imgRoot2 = if (imgRoot1.endsWith("/")) imgRoot1 else imgRoot1+"/"

    b.copy(
      baseUrl   = if (b.baseUrl.endsWith("/")) b.baseUrl.init else b.baseUrl,
      imageRoot = imgRoot2,
      defaultUser = if (b.defaultUser.isEmpty) null else b.defaultUser
    )
  }

  val invalidLinkMarker: String = "@@INVALIDLINK@@"

  private def spaceSeparatedStrings(str: String): Seq[String] = str match {
    case "" => Seq()
    case str if str(0) == '"' =>
      val (s, rest) = str.drop(1).span(_ != '"')
      s +: spaceSeparatedStrings(rest.drop(1).trim)
    case _ =>
      val (s, rest) = str.span(!_.isWhitespace)
      s +: spaceSeparatedStrings(rest.trim)
  }
}


case class Article(
  title: String,
  slug: String,
  author: String = null,
  dates: Seq[LocalDateTime] = Seq(), // publishing date + dates of updates
  tags: Tags = Tags(),
  meta: Meta = Meta(),
  rel: Seq[String] = Seq(),
  pub: Seq[String] = Seq(),
  aliases: Seq[String] = Seq(),
  implies: Seq[Tag] = Seq(), // only for tags
  imgtags: Seq[Tag] = Seq(),
  link: String = null,
  license: String = null,
  rawText: Seq[String] = Seq(),
  text: Text = null,
  images: Seq[Image] = Seq(),
  backlinks: Seq[Article] = Seq(),
  similar: Seq[Similar] = Seq(),
  pubArticles: Seq[Article] = Seq(),
  pubBy: Article = null,
  inFeed: Boolean = true,
  next: Article = null,
  prev: Article = null,
  foreighBacklinks: Seq[ForeignBacklink] = Seq()
) {
  val date = if (dates.isEmpty) null else dates.head
  private def prettyDate = if (date == null) "" else "<"+ DateTimeFormatter.ISO_LOCAL_DATE.format(date)+">"
  override def toString = (if (isTag) "Article[Tag]" else "Article")+s"($prettyDate$title)"
  def asSlug: Slug = Slug(slug)
  def isSupertag = meta.isSupertag
  def isTag      = meta.isTag || meta.isSupertag
  def asTag      = if (isTag) Tag(title, isSupertag) else null
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
    val nozoom = meta.nozoom
    mapImages { i => i.copy(
      tags = i.tags.copy(visible = i.tags.visible.filter(t => !ts.contains(t))),
      zoomable = !nozoom
    )}
  }

  def mkRawText = rawText.mkString("\n")

  override def hashCode: Int = sys.error("articles should not be used as hashmap keys")
}

case class ForeignBacklink(url: String, title: String, site: String)
case class Similar(article: Article, commonTags: Double, dateDiff: Int)

case class Meta(values: Seq[String] = Seq()) {
  val kvPairs: Map[String, String] = values.filter(_.indexOf(':') >= 0).map { x => val Array(k, v) = x.split(":", 2); (k, v) }.toMap
  def value(key: String): String = kvPairs.getOrElse(key, null)
  def contains(x: String) = values.contains(x)
  def merge(that: Meta): Meta = Meta(values ++ that.values)

  def isSupertag = contains("supertag")
  def isTag      = contains("tag")
  def nozoom     = contains("nozoom") // TODO better name?
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
  zoomable: Boolean = true,
  localSource: Article = null
) {
  def asSmallThumbnail = copy(mods = "", align = "")

  val thumb = {
    // ....../image-filaneme.jpg
    //       ^ lastSlash    ^ lastDot
    val sb = new java.lang.StringBuilder
    var lastSlash = url.lastIndexOf('/')
    val containsSlashes = lastSlash != -1
    if (lastSlash == -1) lastSlash = url.length

    if (containsSlashes) {

      var lastDot = url.lastIndexOf('.')
      if (lastDot < lastSlash) lastDot = url.length

      // append first letters of url "tokens"
      var out = false // if thrue, appends next character
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
      sb.append(url, lastSlash+1, lastDot) // append filename without suffix

    } else { // there's no slash in the url, this should not happen TODO
      var lastDot = url.lastIndexOf('.')
      if (lastDot == -1) lastDot = url.length
      sb.append(url, 0, lastDot)
    }

    if (sb.length > 0) {
      sb.append("-")
    }
    sb.append(url.hashCode.toHexString)

    sb.toString
  }
}

case class Slug(id: String)
case class Tags(visible: Seq[Tag] = Seq(), hidden: Seq[Tag] = Seq()) {
  def merge(t: Tags) = Tags(visible ++ t.visible, hidden ++ t.hidden)
  def isEmpty = visible.isEmpty && hidden.isEmpty
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

  lazy val allTags: Map[Tag, (Article, Seq[Article])] = { // [tag -> (article reprsenting this tag, articles tagged by this tag)]
    val imageTagMap: Map[Tag, Seq[(Image, Article)]] = invert(all.flatMap(a => a.images.map(i => ((i, a), i.tags.visible.distinct))))
    def taggedImages(t: Tag) = imageTagMap.getOrElse(t, Seq()).map { case (i, a) => i.copy(localSource = a) } // TODO

    all.filter(_.isTag).map { t =>
      t.asTag -> (t.copy(images = t.images ++ taggedImages(t.asTag)), tagMap.getOrElse(t.asTag, Seq()))
    }.toMap
  }

  def tagByTitle(t: Tag): Article = allTags(t)._1

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





object MakeFiles {

  private val keyVal: PartialFunction[String, (String, String)] = {
    case s if s.split("\\s+", 2).length == 2 =>
      val Array(k, v) = s.split("\\s+", 2); (k, v)
    case s if !s.contains(" ") && s.endsWith("!") =>
      (s, "")
  }

  private val thisDir: File = {
    val dot = new File(".")
    val cp  = System.getProperty("java.class.path")
    val jar = cp.split(":|;").find(_.contains("asciiblog.jar"))
    if (jar.isEmpty) sys.error("there's no asciiblog.jar file on classpath")
    def jarDir = new File(jar.get).getParentFile
    if (new File(dot, "gallery.js").exists()) dot else jarDir
  }

  private def file(f: String) = new File(thisDir, f)

  private def crudelyMinify(js: String) =
    js.replaceAll("""(?<!let|function|in|of|return)\s+(?!in|of)|/\*.*?\*/|//.*\n""", "")
      .replaceAll(""";}""", "}")

  def keyValuesIterator(f: File, enc: String) = io.Source.fromFile(f, enc).getLines.collect(keyVal)

  lazy val galleryScript  = crudelyMinify(io.Source.fromFile(file("gallery.js"), "utf8").mkString)
  lazy val commentsScript = io.Source.fromFile(file("comments.php")).mkString
  lazy val shareScript    = io.Source.fromFile(file("share.php")).mkString

  def readConfig(cfgFile: File): Map[String, String] = {
    // read only the first line and interpret it as utf-8 string
    val is = new FileInputStream(cfgFile)
    val bytes = mutable.ArrayBuffer[Byte]()
    var b = is.read()
    while (b != -1 && b != '\r' && b != '\n') {
      bytes += b.toByte
      b = is.read()
    }
    val firstLine = new String(bytes.toArray, "utf-8")
    is.close()

    // config file may start by "encoding" directive determining encoding for the file itself
    // (This is ok because file either starts with encoding directive or is encoded as utf-8)
    val cfgEncoding = keyVal.lift(firstLine).collect { case ("encoding", v) => v }.getOrElse("utf-8")

    val _cfg = mutable.Map[String, String]()
    val iter = keyValuesIterator(cfgFile, cfgEncoding)
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

  def initBlog(args: Array[String]): Blog = {
    val cfgFile = new File(args(0))
    val cfg = readConfig(cfgFile)
    val txl = keyValuesIterator(file("lang."+cfg.getOrElse("language", "en").trim), "utf-8").toMap
    Blog.populate(cfg, args, txl, cfgFile)
  }

  def init(args: Array[String], f: Blog => Blog = identity): (Blog, Base, String => String, Set[Slug]) =
    makeBase(f(initBlog(args)))

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

      require(baseUrl.startsWith(blog.baseUrl))

      // fast path for absolute foreign urls
      if (!url.startsWith(blog.baseUrl) && (url.startsWith("https://") || url.startsWith("http://"))) {
        return url
      }

      // fast path
      if (url.startsWith(blog.baseUrl)) {
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

      // special case for relative url: count how deep is baseUrl and add as many "../" parts
      // TODO more general
      if (url == "rss.xml") {
        var levels = 0
        var i = blog.baseUrl.length+1; while (i < baseUrl.length) {
          if (baseUrl.charAt(i) == '/') levels += 1
          i += 1
        }
        return if (levels == 0) url else {
          val sb = new java.lang.StringBuilder(url.length+levels*3)
          var i = 0; while (i < levels) {
            sb.append("../")
            i += 1
          }
          sb.append(url).toString
        }
      }

      // General case that can handle everything. Code above can be deleted and
      // everything will work just fine (albeit slowly).
      val link = new URI(url)
      val base = new URI(baseUrl)
      require(base.isAbsolute)

      if (link.getHost != null && link.getHost != base.getHost) {
        url
      } else if (link.getPath == null) { // mailto: links
        url
      } else {
        val us = link.getPath.split("/").filter(_.nonEmpty)
        val bs = base.getPath.split("/").filter(_.nonEmpty)
        val prefixLen = (0 until math.min(bs.length, us.length)).prefixLength { i => us(i) == bs(i) }
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
      if (isLocalLink(url)) {
        val withoutBase = url.drop(blog.baseUrl.length+1)
        val withoutHash = util.splitByHash(withoutBase)._1
        Slug(withoutHash.dropRight(blog.fileSuffix.length))
      } else sys.error("not local url: "+url)

    def addParamMediumFeed(url: String) =
      if (isAbsolute(url) && isLocalLink(url)) addParam(url, "utm_medium=feed") else url

    def thumbnailUrl(img: Image) = s"t/${img.thumb}-${blog.thumbWidth}x${blog.thumbHeight}"+imgSuffix(img)
    def bigThumbnailUrl(img: Image, half: Boolean) = s"t/${img.thumb}-${blog.bigThumbWidth / (if (half) 2 else 1)}"+imgSuffix(img)
    def textCardUrl(a: Article) = s"t/card/${a.slug}.png"
  }



  val titleRegex        = """^(XXX+\s*)?(.+?)(?:\[([^ ]+)\])?$""".r
  private val dateRegex = """^(\d++)-(\d++)-(\d++)(?: (\d++):(\d++)(?::(\d++))?)?""".r
  val licenses = Set("CC by", "CC by-nc", "CC by-nd", "CC by-sa", "CC by-nc-nd", "CC by-nc-sa")

  def parseDates(l: String): Seq[LocalDateTime] = {
    if (!l.charAt(0).isDigit) return null
    val dates = l.split(",").map(l => parseDate(l.trim))
    if (dates.nonEmpty && dates.forall(_ != null)) dates else null
  }

  private def parseDate(l: String): LocalDateTime = {
    val matcher = dateRegex.pattern.matcher(l)
    if (!matcher.matches()) null else _parseDate(matcher)
  }

  private def parseDatePrefix(l: String): (LocalDateTime, String) = {
    val matcher = dateRegex.pattern.matcher(l)
    if (!matcher.lookingAt()) (null, l) else (_parseDate(matcher), l.substring(matcher.end))
  }

  private def _parseDate(matcher: Matcher) = {
    def toInt(s: String) = if (s == null) 0 else s.toInt
    val y  = toInt(matcher.group(1))
    val m  = toInt(matcher.group(2))
    val d  = toInt(matcher.group(3))
    val h  = toInt(matcher.group(4))
    val mi = toInt(matcher.group(5))
    val s  = toInt(matcher.group(6))
    LocalDateTime.of(y, m, d, h, mi, s)
  }

  def parseLicense(l: String): String =
    if (!licenses.contains(l)) null else l

  val tagBlockRegex = """(##|#)\s*(.+?)(?=( |^)(##|#)|$)""".r
  val tagSplitRegex = "\\s*,\\s*".r
  def isBracketed(x: String) = x.length > 2 && x.charAt(0) == '(' && x.charAt(x.length-1) == ')'
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
        if (t.length > 0 && isBracketed(t)) {
          hidden += Tag(t.substring(1, t.length-1), supertag)
        } else {
          visible += Tag(t, supertag)
        }
      }
    }

    Tags(visible.toSeq, hidden.toSeq)
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

  private val slugRegex = """[\w./+%-]+""".r
  private val trailingWS = "\\s+$".r
  private def lastCharIsWhitespace(str: String) =
    str.length > 0 && Character.isWhitespace(str.charAt(str.length-1))

  def parseArticle(lines: Seq[String])(implicit blog: Blog): Article = {
    var ls = if (lines.exists(lastCharIsWhitespace)) lines.map(l => trailingWS.replaceAllIn(l, "")) else lines
    ls = ls.map{ l => if (l.startsWith("\\=")) l.tail else l }

    val titleRegex(xxx, dateTitle, slug) = ls(0)
    val (dateInTitle, title) = parseDatePrefix(dateTitle)

    val (metaLines, b) = ls.drop(2).span(l => l.nonEmpty)
    val body = b.slice(b.indexWhere(_.nonEmpty), b.lastIndexWhere(_.nonEmpty)+1)

    val _metaLines = metaLines.toArray

    val dates   = chompOne(_metaLines, parseDates, Seq.empty)
    val tags    = chompMany(_metaLines, parseTags).fold(Tags()){ _ merge _ }
    val license = chompOne(_metaLines, parseLicense)
    val links   = chompOne(_metaLines, prefixedLine("link:"))
    val authors = chompOne(_metaLines, prefixedLine("by:"), blog.defaultUser)
    val meta    = Meta(chompOne(_metaLines, prefixedList("meta:"), Seq.empty))
    val rels    = chompOne(_metaLines, prefixedList("rel:"), Seq.empty)
    val pubs    = chompOne(_metaLines, prefixedList("pub:"), Seq.empty)
    val aliass  = chompOne(_metaLines, prefixedList("alias:"), Seq.empty)
    val implies = Option(chompOne(_metaLines, prefixedLine("implies:"))).map(x => parseTags(x).visible).getOrElse(Seq.empty)
    val imgtags = Option(chompOne(_metaLines, prefixedLine("imageTags:"))).map(x => parseTags(x).visible).getOrElse(Seq.empty)

    if (unchompedLines(_metaLines) > 0)
      sys.error("some metainformation was not processed: "+_metaLines.toSeq)

    val isTag = meta.contains("supertag") || meta.contains("tag") || (slug != null && isTagSlug(slug))
    val inFeed = xxx == null && !isTag
    val realSlug =
      if (slug != null && slug != "") slug
      else if (isTag) tagSlug(title)
      else generateSlug(title)

    if ((slug == null || slug == "") && blog.demandExplicitSlugs && !title.startsWith("???"))
      sys.error(s"article ${title} is missing explicit slug")

    if (slug != null && slug.nonEmpty && !slugRegex.pattern.matcher(slug).matches())
      sys.error(s"slug '$slug' is not valid, only letters, numbers and ./+- allowed")

    if (links != null)
      require(isAbsolute(links), s"urls in link: field must be absolute ($realSlug)")

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



  def makeRSS(articles: Seq[Article], mkBody: Article => String, selfUrl: String)(implicit blog: Blog): String = {
    val format = DateTimeFormatter.RFC_1123_DATE_TIME
    def rssdate(date: LocalDateTime) = if (date == null) "" else format.format(date.atZone(java.time.ZoneId.systemDefault))

    XMLSW.document { w =>
      w.element("rss", Seq("version" -> "2.0")) { w =>
        w.element("channel") { w =>
          w.element("title", blog.title)
          w.element("description", "")
          w.element("link", selfUrl)
          for (a <- articles) {
            w.element("item") { w =>
              w.element("title", a.title)
              val url = if (a.link == null || a.link.isEmpty) blog.addParamMediumFeed(blog.absUrlFromSlug(a.slug)) else a.link // TODO?
              w.element("guid", Seq(("isPermaLink", "true")), url)
              w.element("pubDate", rssdate(a.date))
              val body = mkBody(a)
              if (body.nonEmpty) {
                w.element("description", body)
              }
              for (t <- a.tags.visible) {
                w.element("category", t.title)
              }
            }
          }
        }
      }
    }.toString
  }



  def readPosts(implicit blog: Blog): Vector[Article] = {
    val lineRegex = """^===+$""".r.pattern
    blog.files.iterator.flatMap { f =>
      var ls = io.Source.fromFile(f, 1024*128)(blog.encoding).getLines.toArray
      val starts = (0 until ls.length).collect { case i if ls(i).startsWith("===") && lineRegex.matcher(ls(i)).matches() => i-1 }
      (0 until starts.length).map { i =>
        parseArticle(ls.slice(starts(i), starts.lift(i+1).getOrElse(ls.length)))
      }
    }.toVector
  }






  def makeBase(blog: Blog, fast: Boolean = false): (Blog, Base, String => String, Set[Slug]) = {
    implicit val _blog = blog
    var articles: Vector[Article] = timer("readfiles", blog)(readPosts(blog))

    articles = blog.hooks.prepareArticles(blog, articles).toVector

    //timer("checks", blog) {
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
    //}

    timer("merge", blog) {
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

    val now = LocalDateTime.now()
    articles = articles
      .filter { a => !hiddenSlugs.contains(a.slug) }
      .filter { a => !blog.excludeFutureArticles || (a.date == null || a.date.isBefore(now)) }

    if (blog.articlesMustBeSorted) { // ordered by date
    timer("articlesMustBeSorted", blog) {
      val dated = articles.filter(_.date != null)
      val ordered = dated == dated.sortBy(_.date).reverse
      if (!ordered) sys.error("articles are not ordered by date")
    }
    }

    timer("tagImplications", blog) {
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

    if (blog.usersAsTags) {
      timer("user lists", blog) {
        articles = articles.map { a =>
          if (a.author == null || a.author.isEmpty || a.author == blog.defaultUser) a else {
            a.copy(tags = a.tags.copy(visible = Tag(a.author) +: a.tags.visible))
          }
        }
      }
    }

    timer("make tags case insensitive", blog) {
      val caseCanonization = articles.iterator
        .flatMap { a => a.tags.visible ++ a.tags.hidden }
        .toVector.distinct
        .groupBy { t => (t.supertag, t.title.toLowerCase) }
        .filter { case (_, variants) => variants.size > 1 }
        .flatMap { case ((supertag, lower), variants) =>
          val canonicalVariant = variants.maxBy(_.title.count(Character.isUpperCase))
          variants.map { v => v -> Tag(canonicalVariant.title, supertag) }
        }.toMap withDefault identity

      articles = articles.map { a =>
        a.copy(tags = a.tags.copy(
          visible = a.tags.visible.map(caseCanonization),
          hidden  = a.tags.hidden.map(caseCanonization)
        ))
      }
    }

    // Maps all article slugs (main ones and aliases) to their absolute urls.
    // Bit of a hack, this lazy function closes over mutable variable articles.
    // So when it's finally called it sould use the most recent and up to date
    // collection of articles. This is needed to resolve circular dependency:
    // global names -> parse text -> materializeNonexplicitTags -> global names
    lazy val globalNames: Map[String, String] = timer("global names", blog) {
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
        if (blog.printErrors) {
          println(s"bad link [$link] (in ${if (a == null) null else a.slug})")
        }
        Blog.invalidLinkMarker
      } else {
        globalMapping(b)+h
      }
    }

    val resolver = (link: String) => resolveLink(link, globalNames, null)

    timer("parse text", blog) {
    articles = articles.map { a =>
      val resolver = new LinkResolver {
          def link(l: String): String = resolveLink(l, globalNames, a)
          def thumbnail(img: Image): String = blog.thumbnailUrl(img)
          def bigThumbnail(img: Image, half: Boolean): String = blog.bigThumbnailUrl(img, half)
      }
      val txt = blog.markup.process(a.rawText, resolver, blog.imageRoot)

      a.copy(
        text = txt,
        images = (a.images ++ txt.images) // images might be already populated from readGallery()
      )
    }
    }

    def materializeNonexplicitTags(all: Vector[Article]): Vector[Article] = timer("materializeNonexplicitTags", blog) {
      val explicitTags: Set[Tag] = all.collect { case a if a.isTag => a.asTag }.toSet
      val mentionedTags: Set[Tag] = all.flatMap { a => a.tags.visible ++ a.tags.hidden ++ a.implies ++ a.imgtags ++ a.images.flatMap(i => i.tags.visible ++ i.tags.hidden) }.toSet
      (mentionedTags -- explicitTags).iterator.map { t =>
        Article(t.title, tagSlug(t.title), meta = Meta(Seq(if (t.supertag) "supertag" else "tag")), text = AsciiText.empty)
      }.toVector
    }

    articles = articles ++ materializeNonexplicitTags(articles)

    articles = articles.map { a =>
      if (a.imgtags.isEmpty) a else a.addImageTags(a.imgtags)
    }

    timer("check slugs and alias for uniqueness", blog) {
    val allIds = new mutable.HashMap[String, Article]()
    for (a <- articles; id <- Iterator(a.slug) ++ a.aliases.iterator) {
      if (allIds.contains(id)) { sys.error(s"duplicate slug/alias '${id}', used in $a and ${allIds(id)}") }
      allIds.put(id, a)
    }

    articles foreach { a =>
      a.pub foreach { id => if (!allIds.contains(id)) sys.error(s"id [$id] is not defined (used as pub in article '${a.title})'") }
      a.rel foreach { id => if (!allIds.contains(id)) sys.error(s"id [$id] is not defined (used as rel in article '${a.title})'") }
    }
    }

    timer("canonize pubs and rels", blog) {
    val canonicalSlugs: Map[String, String] = // [alias -> canonical slug]
      invert1(articles.collect { case a if a.aliases.nonEmpty => (a.slug, a.aliases) })

    articles = articles.map { a =>
      a.copy(
        pub = a.pub.map(id => canonicalSlugs.getOrElse(id, id)),
        rel = a.rel.map(id => canonicalSlugs.getOrElse(id, id))
      )
    }
    }


    val backlinks: Map[Slug, Seq[Article]] = timer("backlinks", blog) {
      invert(articles.map { a => (a, a.slugsOfLinkedArticles) })
        .map { case (k, as) => (k, as.distinctBy(_.slug)) }
    }

    val pubsBy: Map[Slug, Article] = timer("pubsBy", blog) {
      invert(articles.map { a => (a, a.pub.map(Slug)) })
        .map { case (k, vs) => (k, vs.minBy(_.date)) }
    }

    // newest first, articles without date last
    val byDate = (a: Article) => (if (a.date == null) LocalDateTime.MIN else a.date)

    articles = timer("populate backlinks and pubBy", blog) {
      val base = Base(articles, null)

      articles.map { a =>
        val bs = backlinks.getOrElse(a.asSlug, Seq())
        val pubBy = pubsBy.getOrElse(a.asSlug, null)

        a.copy(
          dates = if (a.dates.isEmpty && pubBy != null) pubBy.dates.take(1) else a.dates,
          backlinks = bs.sortBy(byDate).reverse,
          pubArticles = a.pub.map(base.bySlug),
          pubBy = pubBy
        )
      }
    }

    if (blog.sortByDate) {
      articles = articles.sortBy(byDate).reverse
    }

    // TODO hack
    if (fast) return (blog, Base(articles, Map()), x => x, null)

    articles = timer("populate similarities", blog) {
      val sim = new Similarities(articles, blog.similarLimit)
      // similar might be already set by some user hook
      articles.map { a => if (a.similar.nonEmpty) a else a.copy(similar = sim(a)) }
    }

    timer("populate next and prev", blog) {
      val feed = articles.filter(a => a.inFeed && !a.isTag)
      val slug2ord = feed.map(_.slug).zipWithIndex.toMap
      articles = articles.map { a =>
        if (a.isTag) a else {
          a.copy(
            next = slug2ord.get(a.slug).flatMap { ord => feed.lift(ord+1) }.getOrElse(null),
            prev = slug2ord.get(a.slug).flatMap { ord => feed.lift(ord-1) }.getOrElse(null)
          )
        }
      }
    }

    val tagMap = timer("tagMap", blog) {
      val tagMap = invert(articles.map { a => (a, (a.tags.visible).distinct) })
      val tagArticle = articles.iterator.collect { case a if a.isTag => (a.asTag, a) }.toMap

      tagMap.map { case (t, as) =>
        val key = tagArticle(t).meta.value("sortby")

        if (key == null) { // sort by order linked in article (order for << >> navigation)
          val sortedAs = if (blog.sortByDate) as.sortBy(byDate).reverse else as
          val linked = tagArticle(t).slugsOfLinkedArticles.distinct
          val tagged = sortedAs.map(_.asSlug)
          val artMap = sortedAs.map(a => (a.asSlug, a)).toMap
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

    val base = blog.hooks.updateBase(Base(articles, tagMap), blog)
    val changed = timer("detect changed articles", blog) {
      changedArticles(base, blog)
    }

    (blog, base, resolver, changed)
  }




  def changedArticles(base: Base, blog: Blog): Set[Slug] = {
    import scala.util.hashing.MurmurHash3.{ mix, finalizeHash, mapHash }

    def hashCode(a: Article) = {
      var h = 0xcafebabe
      h = mix(h, a.title.hashCode)
      h = mix(h, a.slug.hashCode)
      h = mix(h, a.author.##)
      h = mix(h, a.dates.##)
      h = mix(h, a.tags.##)
      h = mix(h, a.meta.##)
      //h = mix(h, a.rel.##)
      h = mix(h, a.pub.##)
      h = mix(h, a.aliases.##)
      h = mix(h, a.implies.##)
      h = mix(h, a.imgtags.##)
      h = mix(h, a.link.##)
      h = mix(h, a.license.##)
      h = mix(h, a.rawText.##)
      //h = mix(h, a.text.##) // contains reference to globalNames
      //h = mix(h, a.images.##)
      h = skeletonHashCodes(a.backlinks, h)
      h = skeletonHashCodes(a.similar.map(_.article), h)
      h = skeletonHashCodes(a.pubArticles, h)
      h = skeletonHashCode(a.pubBy, h)
      h = mix(h, a.inFeed.##)
      h = skeletonHashCode(a.next, h)
      h = skeletonHashCode(a.prev, h)
      finalizeHash(h, 99)
    }

    def skeletonHashCode(a: Article, _h: Int): Int = {
      var h = _h
      if (a != null) {
        h = mix(h, a.title.##)
        h = mix(h, a.slug.##)
      }
      h
    }

    def skeletonHashCodes(as: Seq[Article], _h: Int): Int = {
      var h = _h
      for (a <- as) h = skeletonHashCode(a, h)
      h
    }

    val cacheFile = new File(blog.outDir, ".articleHashes")

    def readHashes(): (Int, Map[String, Int]) = {
      val Vector(cfgHash, lines @ _*) = io.Source.fromFile(cacheFile, "utf-8").getLines.toVector
      val hashes =
        lines.map { line =>
          val Array(hash, s) = line.split(" ", 2)
          (s, hash.toInt)
        }.toMap

      (cfgHash.toInt, hashes)
    }

    def writeHashes(cfgHash: Int, hashes: Map[String, Int]) = {
      val fw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(cacheFile), "utf-8"))
      fw.write(cfgHash+"\n")
      for ((s, hash) <- hashes.toSeq.sortBy(_._1))  {
        fw.write(hash+" "+s+"\n")
      }
      fw.close()
    }

    def all = base.all.map(_.asSlug).toSet

    if (!blog.trackChangedArticles) {
      cacheFile.delete()
      return all
    }

    val (newCfgHash, newHashes) = (mapHash(blog.cfg), base.all.map { a => (a.slug, hashCode(a)) }.toMap)
    val (oldCfgHash, oldHashes) = if (!cacheFile.exists) (0, Map[String, Int]()) else readHashes()

    writeHashes(newCfgHash, newHashes)

    if (newCfgHash != oldCfgHash) return all

    val invalidSlugs = mutable.Set[String]()
    invalidSlugs ++= (newHashes.keySet -- oldHashes.keys)

    for ((s, newHash) <- newHashes) {
      if (oldHashes.contains(s) && oldHashes(s) != newHash) {
        invalidSlugs += s
      }
    }

    invalidSlugs.map(Slug).toSet
  }





  class Saver(blog: Blog, changedSlugs: Set[Slug], oldFileIndex: Map[String, String]) {
    val fileIndex = collection.concurrent.TrieMap[String, String]()
    def apply(articles: Seq[Article], f: String)(content: => String) = {
      if (articles == null || articles.exists(a => changedSlugs.contains(a.asSlug)) || !oldFileIndex.contains(f)) {
        fileIndex += saveFile(f, content, oldFileIndex)(blog)
      } else {
        fileIndex += (f -> oldFileIndex(f))
      }
    }

    def saveFile(f: String, content: String, fileIndex: Map[String, String] = Map())(implicit blog: Blog): (String, String) = { // filename -> hash
      val ff = new File(blog.outDir, f)

      val p = ff.getParentFile
      if (p != null) p.mkdirs()

      val h = hash(content)

      if (!fileIndex.contains(f) || fileIndex(f) != h || !ff.exists) {
        val fw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(ff), "utf-8"))
        fw.write(content)
        fw.close()
      }

      f -> h
    }
  }




  def makeFiles(blog: Blog, base: Base, resolver: String => String, changedSlugs: Set[Slug]) = try {
    implicit val _blog = blog

    val layoutMill: LayoutMill = new FlowLayoutMill(base, blog, resolver)

    val save = new Saver(blog, changedSlugs, {
      val f = new File(blog.outDir, ".files")
      if (!f.exists) Map()
      else io.Source.fromFile(f).getLines.map { l => val Array(k, v) = l.split(" ", 2); (v, k) }.toMap
    })


    val archivePages: Seq[(Article, Seq[Article])] = timer("group archive", blog) {

      def chunk(as: Vector[Article])(g: LocalDateTime => (Int, Int))(f: ((Int, Int)) => Article): Vector[(Article, Vector[Article])] =
        as.groupBy { a => if (a.date == null) (0,0) else g(a.date) }.toVector.sortBy(_._1).reverse.map { case (t, as) => (f(t), as) }
      def mkDate(y: Int, m: Int) = if (y == 0) Seq() else Seq(LocalDateTime.of(y, m, 1, 0, 0, 0))

      val title = blog.translation("archive")
      val undated = blog.translation("undated")

      def yearmonth(d: LocalDateTime) = (d.getYear, d.getMonthValue)
      def yearJanuary(d: LocalDateTime) = (d.getYear, 1)

      blog.groupArchiveBy match {
        case "month" =>
          chunk(base.feed)(yearmonth) { case (y, m) =>
            Article(if (y > 0) s"$title $m/$y" else s"$title ($undated)", s"index-$y-$m", dates = mkDate(y, m))
          }

        case "year" =>
          chunk(base.feed)(yearJanuary) { case (y, _) =>
            Article(if (y > 0) s"$title $y" else s"$title ($undated)", s"index-$y", dates = mkDate(y, 1))
          }

        case "none" =>
          Vector()

        case num if num.matches("\\d+") =>
          val len = num.toInt
          base.feed.reverse.grouped(len).toVector.zipWithIndex.map { case (as, i) =>
            (Article(s"$title #${i*len+1}-${(i+1)*len}", "index-"+(i+1)), as.reverse)
          }.reverse
      }
    }


    def lastTags(count: Int, days: Int) = {
      val yearAgo = LocalDateTime.now().minusDays(days)
      val thisYearArticles = base.all.filter(a => a.date != null && a.date.isAfter(yearAgo))
      val articles = if (thisYearArticles.size >= 30) thisYearArticles else base.feed.take(30)

      articles
        .flatMap(_.tags.visible)
        .groupBy(a => a).toSeq
        .filter { case (_, ts) => ts.size >= 3 }
        .sortBy { case (_, ts) => ~ts.size }
        .take(count)
        .map { case (t, _) => base.allTags(t)._1 }
    }


    def mkPagePart(cfg: String, articles: Seq[Article], layout: Layout, base: Base) =
      cfg.split("\\s+") match {
        case Array(part @ ("full" | "fullArticles" | "summary" | "summaries" | "link" | "links" | "body" | "bodies"), args @ _*) =>

          val as: Seq[Article] = if (args.forall(util.isInt)) {
            val (from, until) = args.map(_.toInt) match {
              case Seq(f, u) => (f, u)
              case Seq(u)    => (0, u)
              case Seq()     => (0, Int.MaxValue/4)
            }
            articles.slice(from, until)
          } else {
            args.map(base.bySlug)
          }

          part match {
            case "full" | "fullArticles" => PagePart.FullArticles(as)
            case "summary" | "summaries" => PagePart.Summaries(as)
            case "link" | "links"        => PagePart.Links(as)
            case "body" | "bodies"       => PagePart.Bodies(as)
          }

        case Array("text", _*) =>
          PagePart.Text(cfg)

        case Array("tags", args @ _*) =>
          val Seq(cnt, days) = Seq.tabulate(2) { args.map(_.toInt).orElse(Seq(20, 365)) }
          PagePart.Tags(lastTags(cnt, days))

        case Array("archive") => PagePart.Archive(archivePages.map(_._1), blog.groupArchiveBy)

        case Array(part, args @ _*) =>
          blog.hooks.makePagePart(base, blog, layout, part, args.mkString(" "))
      }

    def mkBody(cfg: Seq[String]): Article => String =
      cfg.head.split("\\s+").head match {
        case "full" | "fullArticles" => a => FlowLayout.updateLinks(a.text.render(identity), blog.addParamMediumFeed)
        case "summary" | "summaries" => a => FlowLayout.updateLinks(a.text.plaintextSummary, blog.addParamMediumFeed)
        case "link" | "links"        => a => ""
      }

    val rssLimit = blog.rssCfg.head.split("\\s+").lift(1).getOrElse("10").toInt



    timer("generate and save files", blog) {
    timer("generate and save files - archive", blog) {
    val archiveLinks = archivePages.zipWithIndex.map { case ((a, as), idx) =>
      save(as, blog.relUrl(a)) {
        val l = layoutMill.make(blog.absUrl(a))
        val prev = archivePages.lift(idx+1).map(_._1).getOrElse(null)
        val next = archivePages.lift(idx-1).map(_._1).getOrElse(null)
        val parts = blog.archiveCfg.map(cfg => mkPagePart(cfg, as, l, base))
        val body = l.makeArchive(a.copy(prev = prev, next = next), parts)
        l.makePage(body, containImages = as.exists(_.hasImageMarker))
      }
      a
    }

    val path = blog.relUrlFromSlug("index")
    save(null, path) {
      val l = layoutMill.make(blog.absUrlFromPath(path))
      val indexParts = blog.indexCfg.map(cfg => mkPagePart(cfg, base.feed, l, base))
      val body = l.makeIndex(indexParts)
      l.makePage(body, containImages = true) // TODO
    }
    }

    timer("generate and save files - articles", blog) {
    base.articles foreach { a =>
      if (a.link == null || a.link.isEmpty) { // TODO?
        save(Seq(a), blog.relUrl(a)) {
          var l = layoutMill.make(blog.absUrl(a))
          val aa = a.imagesWithoutArticleTags
          val body = l.makeFullArticle(aa)
          l.makePage(body, aa.title, containImages = aa.images.exists(_.zoomable), ogTags = aa)
        }
      }
    }
    }

    timer("generate and save files - tags", blog) {
    base.allTags foreach { case (t, (a, as)) =>
      var l = layoutMill.make(blog.absUrl(a))

      val linked = a.slugsOfLinkedArticles(blog).toSet
      val list = as.filter(a => !linked.contains(a.asSlug) && !a.isTag)
      val parts = blog.tagsCfg.map(cfg => mkPagePart(cfg, list, l, base))

      val body = l.makeFullArticle(a.imagesWithoutArticleTags, parts)
      val hasImages = a.images.nonEmpty || as.exists(_.images.nonEmpty)

      save(as :+ a, blog.relUrl(a))(l.makePage(body, a.title, containImages = hasImages, rssLink = a.slug+".xml"))
      save(as :+ a, a.slug+".xml")(makeRSS(as.take(rssLimit), mkBody(blog.rssTagsCfg), blog.absUrlFromPath(a.slug+".xml")))
    }

    {
      val path = blog.relUrlFromSlug("tags")
      val l = layoutMill.make(blog.absUrlFromPath(path))
      val tags: Seq[(Article, Int)] = base.allTags.values.toVector
        .filter { case (t, as) => t.images.size > 0 || as.size > 0 }
        .map    { case (t, as) => (t, as.size) }
        .sortBy { case (t, c)  => (~c, t.slug) }
      save(null, path)(l.makePage(l.makeTagIndex(tags)))
    }
    }

    save(null, "rss.xml")(makeRSS(base.feed.take(rssLimit), mkBody(blog.rssCfg), blog.absUrlFromPath("rss.xml")))

    if (blog.allowComments) {
      val l = layoutMill.make(blog.absUrlFromPath("comments.php"))
      val p = l.makePage("{comments.body}", null, false,  null, includeCompleteStyle = true)
      val Array(pre, post) = p.split(Regex.quote("{comments.body}"))

      save(null, "comments.php") {
        val replacements = blog.translation.collect { case (k, v) if k.startsWith("comments.") => s"{$k}" -> v }.toSeq ++ Seq(
          "{comments.prebody}"    -> pre,
          "{comments.postbody}"   -> post,
          "{comments.baseUrl}"    -> blog.baseUrl,
          "{comments.moderation}" -> blog.commentsModeration.toString,
          "href=rss.xml"          -> """href="'.escapeHtmlAttr($requestUrl).'&amp;rss"""" // this is a bit ugly trick
        )
        replacements.foldLeft(commentsScript) { case (cs, (from, to)) => cs.replace(from, to) }
      }
      new File(".comments").mkdirs()
      save(null, ".comments/.htaccess")("Deny from all")
      save(null, ".comments/articles")(base.all.sortBy(_.slug).map { a => a.slug+" "+a.title }.mkString("\n"))
    }

    if (blog.allowShareScript) {
      val urlPattern = URLEncoder.encode(blog.absUrlFromSlug("SLUG"), "UTF-8").replace("SLUG", "${slug}")
      save(null, "share.php") { shareScript.replace("{slug}", urlPattern) }
    }

    if (blog.cssFile != null) {
      save(null, "style.css")(io.Source.fromFile(blog.cssFile).mkString)
    } else if (blog.cssExport) {
      save(null, "style.css")(CSSMinimizeJob(layoutMill.basicStyle + "\n" + blog.cssStyle).fullMinimizedStyle)
    }

    if (blog.galleryScriptExport) {
      save(null, "gallery.js")(galleryScript)
    }

    save(null, "robots.txt")("User-agent: *\nAllow: /")
    save(null, ".files")(save.fileIndex.toSeq.sorted.map { case (file, hash) => hash+" "+file }.mkString("\n"))
    }


    timer("resize images", blog) {
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

    import java.awt.Color

    def createCard(a: Article, w: Int = 480, h: Int = 240, background: Color = Color.WHITE, foreground: Color = Color.BLACK) = {
      val dateFormat = DateTimeFormatter.ofPattern("d. M. yyyy")
      val line1 = (if (a.isTag) "#" else "")+a.title
      val line2 = blog.twitterSite+(if (a.date != null) " — "+dateFormat.format(a.date) else "")
      ImageTools.createCardImage(line1, line2, w, h, background, foreground)
    }

    if (blog.textCards) {
      for (a <- base.all) {
        val dest = new File(blog.outDir, blog.textCardUrl(a))
        if (!dest.exists) {
          dest.getParentFile.mkdirs()
          val img = createCard(a, background = blog.textCardsBackground, foreground = blog.textCardsColor)
          ImageIO.write(img, "png", dest)
        }
      }
    }

    for ((image, jobs) <- resizeJobs) {
      try {
        lazy val fullImage = {
          if (blog.localImages && image.url.startsWith(blog.imageRoot)) {
            val path = URLDecoder.decode(image.url.drop(blog.imageRoot.length), "utf8")
            val localFile = new File(blog.imageDir, path)
            println(s"resizing local image ${localFile}")
            ImageIO.read(localFile)
          } else {
            println(s"resizing image ${image.url}")
            ImageIO.read(new URL(image.url))
          }
        }

        for ((thumbFile, w, h, sharpenStrength) <- jobs if !thumbFile.exists) {
          val fi = fullImage
          println(s"into $thumbFile")
          fi match {
            case null => println(s"ImageIO.read(${image.url}) == null")
            case full =>
              val suffix = image.url.split("\\.").last.toLowerCase
              val s = if (ImageIO.getWriterFileSuffixes.contains(suffix)) suffix else "jpg"
              val leeway = if (s == "png" || s == "gif") 1.6 else 1.6
              val strength = if (s == "png" || s == "gif") 0f else sharpenStrength
              val resized = ImageTools.resizeImage(full, w, h, leeway, strength)
              if (s == "jpg" || s == "jpeg") {
                ImageTools.saveJpg(thumbFile, resized, 0.85f)
              } else {
                ImageIO.write(resized, s, thumbFile)
              }
          }
        }
      } catch {
        case e: IIOException =>
          println(e)
          println(s"note: you might want to set `localImages false` in your config file in order to fetch all images from server (${blog.imageRoot})")
          println(s"note: you might want to set `localImages true` in your config file in order to fetch all images from local directory (${blog.imageDir})")
      }
    }
    }


  if (blog.printTiming) {
    println(T.t)
  }

  } catch {
    case e: Exception =>
      // If anything goes wrong nuke .files index.
      // It's necessary for correct conditional save in `save` method.
      // In cases when I make some changes, program may overwrite few files and
      // than explode without updating .files index. Then if I revert that
      // change, program incorrectly thinks (according to index) that all files
      // are unchanged and it not save correct version. Deleting .files index
      // prevents this problem, because after every error, all files are
      // regenerated.
     new File(blog.outDir, ".files").delete()
     new File(blog.outDir, ".articleHashes").delete()
     throw new Exception(e)
  }
}
