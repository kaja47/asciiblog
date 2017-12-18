package asciiblog

import MakeFiles.{ hash, tagSlug, invert, undefId, UrlOps, isAbsolute }
import java.io.{ File, FileWriter, FileOutputStream }
import java.net.{ URL, URI, HttpURLConnection, UnknownHostException }
import java.security.MessageDigest
import java.text.SimpleDateFormat
import java.util.{ Date, GregorianCalendar, Calendar, Locale, zip }
import javax.imageio.{ ImageIO, IIOException }
import scala.collection.{ mutable, immutable }
import scala.util.matching.Regex
import util._

case class Blog (
  val title: String,
  val baseUrl: String,
  val files: Seq[String],
  val outDir: String,
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

  val openGraph: Boolean,
  val twitterSite: String,
  val twitterCreator: String,

  val args: Array[String],
  val translation: Map[String, String]
) {
  def hasOgTags = twitterSite.nonEmpty || twitterCreator.nonEmpty || openGraph
  def printTimes: Boolean  = false
  def printErrors: Boolean = !(args.length > 1 && args(1) == "tags")
}

object Blog {
  def populate(cfg: Map[String, String], args: Array[String], translation: Map[String, String]) = {
    new Blog(
      title                  = cfg("title"),
      baseUrl                = cfg("baseUrl"),
      files                  = spaceSeparatedStrings(cfg.getOrElse("files", "").trim),
      outDir                 = cfg.getOrElse("outDir", null),
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

      openGraph              = cfg.getOrElse("openGraph", "false").toBoolean,
      twitterSite            = cfg.getOrElse("twitter.site", ""),
      twitterCreator         = cfg.getOrElse("twitter.creator", ""),

      args = args,
      translation = translation
    )
  }

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
  dates: Seq[Date] = Seq(), // publishing date + dates of updates
  tags: Tags = Tags(),
  meta: Meta = Meta(),
  link: String = null,
  notes: String = null,
  license: String = null,
  rawText: String = "",
  text: Text = null,
  images: Seq[Image] = Seq(),
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
  def asTag      = if (isTag) Tag(title, isSupertag) else null
  def extraImages = images.filter(!_.inText)
  def rel: Seq[String] = meta.seq("rel")
  def aliases: Seq[String] = meta.scalars.toVector
  def links: Seq[String] = {
    text.links.foreach(l => require(isAbsolute(l), s"Text.links must return absolute urls, '$l' provided (in article $slug)"))
    text.links
  }
  def slugsOfLinkedArticles(implicit blog: Blog): Seq[Slug] = links.filter(blog.isLocalLink).map(blog.extractSlug)

  // image marker is shown only for images that are not from external sources
  def hasImageMarker = images.exists(i => i.source == null || i.source == "")
}

case class Meta(values: Map[String, Meta] = Map()) {
  def scalar(key: String): String = scalars.find(_.startsWith(key+":")).map(_.drop(key.length+1).trim).getOrElse(null)
  def scalars = values.collect { case (k, v) if v == null => k }
  def seq(k: String): Seq[String] = values.get(k).map(_.values.keys.toSeq).getOrElse(Seq())
  def withSeq(k: String, xs: Seq[String]) = Meta(values.updated(k, Meta(xs.map(_ -> null).toMap)))
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
  val thumb = hash(url)
  def asSmallThumbnail = copy(mods = "", align = "")
}

case class Slug(/*blog: Blog, */id: String)
case class Tags(visible: Seq[Tag] = Seq(), hidden: Seq[Tag] = Seq()) {
  def merge(t: Tags) = Tags(visible ++ t.visible, hidden ++ t.hidden)
}
case class Tag(title: String, supertag: Boolean = false) {
  override def toString = "Tag("+hashTag+")"
  def hashTag = (if (supertag) "##" else "#")+title
}


case class Base(all: Vector[Article], _tagMap: Map[Tag, Seq[Article]] = Map()) {
  private lazy val tagsOfImages: Seq[Tag] = all.flatMap(_.images).flatMap(_.tags.visible)
  lazy val tagMap: Map[Tag, Seq[Article]] = tagsOfImages.map(_ -> Seq()).toMap ++ _tagMap

  private lazy val imageTagMap: Map[Tag, Seq[(Image, Article)]] = invert(all.flatMap(a => a.images.map(i => ((i, a), i.tags.visible.distinct))))
  private def taggedImages(t: Tag) = imageTagMap.getOrElse(t, Seq()).map { case (i, a) => i.copy(inText = false, localSource = a) }

  lazy val extraTags: Seq[Article] = { // tags that have no particular article
    val direct: Set[Tag] = all.collect { case a if a.isTag => a.asTag }.toSet
    tagMap.collect { case (t, as) if !direct.contains(t) =>
      Article(t.title, tagSlug(t.title), meta = Meta(Map((if (t.supertag) "supertag" else "tag") -> null)), text = AsciiText.empty)
    }.toSeq
  }

  private lazy val bySlug: Map[String, Article] = (all ++ extraTags).map(a => (a.slug, a)).toMap
  private lazy val byMeta: Map[String, Article] = (all ++ extraTags).flatMap(a => a.meta.scalars collect { case m: String => (m, a) }).toMap

  lazy val articles = all.filter(a => !a.isTag)
  lazy val feed = all.filter(a => a.inFeed && !a.isTag)

  lazy val allTags: Map[Article, Seq[Article]] =
    (all ++ extraTags).filter(_.isTag).map { t => (t.copy(images = t.images ++ taggedImages(t.asTag)), tagMap.getOrElse(t.asTag, Seq())) }.toMap

  lazy val tagByTitle: Map[Tag, Article] = allTags.map { case (t, _) => (t.asTag, t) }

  private lazy val art2ord = feed.zipWithIndex.toMap

  def find(id: String): Option[Article] = bySlug.get(id).orElse(byMeta.get(id))
  def isValidId(id: String): Boolean = find(id).nonEmpty
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
  private val articlesReferencedByRel = base.all.flatMap { a => a.rel.flatMap { rel => base.find(rel).orElse { undefId(rel, a); None } } }
  private val arts: Array[Article] = (tagMap.values.flatten ++ articlesReferencedByRel).toArray.distinct
  private val artMap: Map[Article, Int] = arts.zipWithIndex.toMap
  private val slugMap: Map[Slug, Int] = artMap.map { case (k, v) => (k.asSlug, v) }
  private val tm: Map[Tag, Array[Int]] = tagMap.map { case (t, as) => (t, as.map(artMap).toArray) }

  def similarByTags(a: Article, count: Int, without: Seq[Article]): Seq[Article] = {
    def dateDiff(a: Article, b: Article): Long = {
      (a.date, b.date) match {
        case (null, null) => 0
        case (null, _) | (_, null) => Long.MaxValue/2
        case (a, b) => math.abs(a.getTime - b.getTime)
      }
    }

    val arrs = (a.tags.visible ++ a.tags.hidden).flatMap(tm.get)
    if (arrs.size == 0) return Seq()

    val freq = new Array[Int](arts.length) // article idx -> count
    for (arr <- arrs) {
      var i = 0; while (i < arr.length) {
        freq(arr(i)) += 1
        i += 1
      }
    }

    for (i <- a.rel.flatMap(base.find).flatMap(artMap.get)) {
      freq(i) += 64
    }

    for (a <- without ; i <- slugMap.get(a.asSlug)) freq(i) = 0
    for (i <- slugMap.get(a.asSlug)) freq(i) = 0
    freq(slugMap(a.asSlug)) = 0

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

    val sortedMap = mutable.TreeMap[Key, Article]()
    var min = Key(0,0,0)

    for (i <- 0 until arts.length) if (freq(i) >= 1) {
      val key = Key(freq(i), dateDiff(a, arts(i)), i) // most common tags, published closest together
      if (!o.gt(key, min)) {
        sortedMap.put(key, arts(i))
        if (sortedMap.size > count) {
          val last = sortedMap.last._1
          sortedMap.remove(last)
          min = last
        }
      }
    }

    sortedMap.values.toVector

    //val w = without.map(_.slug).toSet + a.slug
    //val rr = sortedMap.toVector.map { case (Key(f, d, _), a) => ((~f, d), a) }
    //val v = for (i <- 0 until arts.length if freq(i) >= 1 && !w.contains(arts(i).slug)) yield ((~freq(i), dateDiff(a, arts(i))), arts(i))
    //v.sortBy(_._1).map(_._2).take(count)
  }
}



object Make extends App {

  if (args.length < 1) {
    println("config file not specified")
    sys.exit()
  }

  val (blog, markup, base) = MakeFiles.init(args)

  if (args.length >= 2 && args(1) == "tags") {
    MakeFiles.tags(blog, base)

  } else if (args.length >= 2 && args(1) == "checkLinks") {
    MakeFiles.checkUrls(blog, base)

  } else {
    MakeFiles.makeFiles(blog, base, markup)
  }

}



object MakeFiles {

  def keyVal: PartialFunction[String, (String, String)] = {
    case s if s.split(" ", 2).length == 2 =>
      val Array(k, v) = s.split(" ", 2); (k, v)
  }

  private val thisDir = /*new File(".")*/ new File(System.getProperty("java.class.path")).getParent

  def crudelyMinify(js: String) = js.replaceAll("(?<!let|function|in)[\\s]+(?!in)|/\\*.*?\\*/", "")
  def keyValues(f: String) = io.Source.fromFile(f).getLines.collect(keyVal).toMap

  lazy val galleryScript  = crudelyMinify(io.Source.fromFile(thisDir+"/gallery.js").mkString)
  lazy val commentsScript = io.Source.fromFile(thisDir+"/comments.php").mkString
  lazy val outScript      = io.Source.fromFile(thisDir+"/out.php").mkString

  def init(args: Array[String]) = {
    val cfg = keyValues(args(0))
    val txl = keyValues(thisDir+"/lang."+cfg.getOrElse("language", "en"))
    val blog = Blog.populate(cfg, args, txl)
    val markup = AsciiMarkup
    val base = makeBase(blog, markup)
    (blog, markup, base)
  }


  def invert[A, B](m: Seq[(A, Seq[B])]): Map[B, Seq[A]] =
    m.flatMap { case (a, bs) => bs.map { b => (b, a) }  }
      .groupBy(_._1)
      .map { case (b, bas) => (b, bas.map(_._2).toVector) }


  def isAbsolute(url: String) = new URI(fixPath(url)).isAbsolute
  def fixPath(path: String) = path.replaceAll(" ", "%20") // TODO hackity hack
  def addParam(url: String, param: String) =
    if (url.contains("?")) url+"&"+param
    else                   url+"?"+param
  def imgSuffix(img: Image) = {
    val imageSuffixes = Set("jpg", "jpeg", "png")
    val suffix = img.url.split("\\.").last.toLowerCase
    if (imageSuffixes.contains(suffix)) "."+suffix else ""
  }

  implicit class UrlOps(blog: Blog) {
    def absUrlFromPath(path: String) = blog.baseUrl + "/" + path
    def absUrlFromSlug(slug: String) = blog.baseUrl + "/" + slug + blog.fileSuffix
    def relUrlFromSlug(slug: String) = slug + blog.fileSuffix
    def absUrl(a: Article) = absUrlFromSlug(a.slug)
    def relUrl(a: Article) = relUrlFromSlug(a.slug)
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
        (Seq.fill(backLevels)("..") ++ us.drop(prefixLen)).mkString("/") +
          (if (u.getRawQuery != null) "?"+u.getRawQuery else "")+
          (if (u.getRawFragment != null) "#"+u.getRawFragment else "")
      }
    }

    def isLocalLink(url: String) = url.startsWith(blog.baseUrl+"/")
    def dropLocalPrefix(url: String) = url.drop(blog.baseUrl.length+1)
    def extractSlug(url: String) = if (isLocalLink(url)) Slug(dropLocalPrefix(url).dropRight(blog.fileSuffix.length)) else sys.error("not local url")

    def addParamMediumFeed(url: String) =
      if (isAbsolute(url) && isLocalLink(url)) addParam(url, "utm_medium=feed") else url

    def thumbnailUrl(img: Image) = s"t/${img.thumb}-${blog.thumbWidth}x${blog.thumbHeight}"+imgSuffix(img)
    def bigThumbnailUrl(img: Image, half: Boolean) = s"t/${img.thumb}-${blog.bigThumbWidth / (if (half) 2 else 1)}"+imgSuffix(img)

  }



  def undefId(id: String, a: Article) = {
    println(s"id [$id] is not defined in article '${a.title}'")
    "undefined id "+id
  }


  private val titleRegex    = """^(XXX+\s*)?(.+?)(?:\[([^ ]+)\])?$""".r
  private val dateRegex     = """^(\d+)-(\d+)-(\d+)(?: (\d+):(\d+)(?::(\d+))?)?""".r
  val licenses = Set("CC by", "CC by-nc", "CC by-nd", "CC by-sa", "CC by-nc-nd", "CC by-nc-sa")

  def parseDates(l: String): Option[Seq[Date]] = {
    if (!l.charAt(0).isDigit) return None
    val dates = l.split(",").map(l => parseDate(l.trim))
    if (dates.nonEmpty && dates.forall(_.isDefined)) Some(dates.map(_.get)) else None
  }

  def parseDate(l: String): Option[Date] = l match {
    case dateRegex(y, m, d, null, null, null) =>
      Some(new GregorianCalendar(y.toInt, m.toInt-1, d.toInt).getTime)
    case dateRegex(y, m, d, h, mi, null) =>
      Some(new GregorianCalendar(y.toInt, m.toInt-1, d.toInt, h.toInt, mi.toInt, 0).getTime)
    case dateRegex(y, m, d, h, mi, s) =>
      Some(new GregorianCalendar(y.toInt, m.toInt-1, d.toInt, h.toInt, mi.toInt, s.toInt).getTime)
    case _ => None
  }

  val parseLicense: PartialFunction[String, String] = {
    case l if licenses.contains(l) => l
  }

  val tagBlockRegex = """(##|#)\s*(.+?)(?=( |^)(##|#)|$)""".r
  val tagSplitRegex = "\\s*,\\s*".r
  def isBracketed(x: String) = x.charAt(0) == '(' && x.charAt(x.length-1) == ')'
  def getTags(s: String) =
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
  def peelOffTags(s: String): (String, Tags) = (tagBlockRegex.replaceAllIn(s, "").trim, getTags(s))
  val parseTags: PartialFunction[String, Tags] = {
    case s if s.charAt(0) == '#' => getTags(s)
  }

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

  val parseLink: PartialFunction[String, String] = {
    case l if l.startsWith("link:") => l.drop("link:".length).trim
  }

  val parseNotes: PartialFunction[String, String] = {
    case l if l.startsWith("notes:") => l.drop("notes:".length).trim
  }

  def hash(txt: String): String = hash(txt.getBytes("utf-8"))
  def hash(txt: Array[Byte]): String = BigInt(1, _md5(txt)).toString(16).reverse.padTo(32, '0').reverse
  def _md5(bytes: Array[Byte]) = MessageDigest.getInstance("MD5").digest(bytes)

  private val trailingWS = "\\s*$".r

  def parseArticle(lines: Seq[String])(implicit blog: Blog): Article = {
    val ls = lines.map(l => if (l.length > 0 && l(l.length-1).isWhitespace) trailingWS.replaceAllIn(l, "") else l)

    val titleRegex(xxx, title, slug) = ls(0)

    val (metaLines, b) = ls.drop(2).span(l => l.nonEmpty)
    val body = b.slice(b.indexWhere(_.nonEmpty), b.lastIndexWhere(_.nonEmpty)+1)

    val dates   = metaLines.flatMap(parseDates)
    val tags    = metaLines.collect(parseTags)
    val license = metaLines.collect(parseLicense)
    val links   = metaLines.collect(parseLink)
    val metas   = metaLines.flatMap(parseMeta(_, "meta: "))
    val notess  = metaLines.collect(parseNotes)
    val meta = metas.foldLeft(Meta())(_ merge _)

    val isTag = meta.values.contains("supertag") || meta.values.contains("tag") || (slug != null && isTagSlug(slug))
    val inFeed = blog.dumpAll || (xxx == null && !isTag)
    val realSlug =
      if (slug != null && slug != "") slug else
        if (isTag) tagSlug(title)
        else generateSlug(title)

    if ((dates.size + tags.size + license.size + links.size + notess.size + metas.size) < metaLines.size)
      sys.error("some metainformation was not processed: "+metaLines)

    links.foreach(l => require(isAbsolute(l), s"urls in link: field must be absolute ($realSlug)"))

    new Article(
      title   = title.trim,
      slug    = realSlug,
      dates   = dates.flatten,
      tags    = tags.fold(Tags()) { (a, b) => a.merge(b) },
      meta    = meta,
      link    = links.headOption.getOrElse(null),
      notes   = notess.headOption.getOrElse(null),
      license = license.headOption.getOrElse(null),
      rawText = body.mkString("\n"),
      inFeed  = inFeed
    )
  }

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

  def year(d: Date) = calendar(d, Calendar.YEAR)
  def month(d: Date) = calendar(d, Calendar.MONTH)+1
  def yearmonth(d: Date) = (year(d), month(d))



  def rssdate(date: Date) = if (date == null) "" else
    new SimpleDateFormat("EEE', 'dd' 'MMM' 'yyyy' 'HH:mm:ss' 'Z", Locale.US).format(date)

  def makeRSS(articles: Seq[Article], mkBody: Article => String)(implicit blog: Blog): String =
    "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n" + (
  <rss version="2.0">
  <channel>
  <title>{blog.title}</title>{
    articles.map(a => <item>
      <title>{a.title}</title>
      <guid isPermaLink="true">{blog.addParamMediumFeed(blog.absUrlFromSlug(a.slug))}</guid>
      <pubDate>{rssdate(a.date)}</pubDate>
      {if (mkBody != null) <description>{mkBody(a)}</description> else xml.NodeSeq.Empty }
    </item>)
  }</channel>
  </rss>).toString



  def saveFile(f: String, content: String, fileIndex: Map[String, String] = Map())(implicit blog: Blog): Seq[(String, String)] = { // filename -> hash
    val ff = new File(blog.outDir, f)

    val p = ff.getParentFile
    if (p != null) p.mkdirs()

    val h = hash(content)

    //if (!fileIndex.contains(f) || fileIndex(f) != h || !ff.exists) {
      val fw = new FileWriter(ff)
      fw.write(content)
      fw.close()
    //}

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

      val imgUrls = albumDir.list collect { case f if f.toLowerCase.endsWith(".jpg") =>
        fixPath(blog.baseUrl+"/albums/"+albumDir.getName+"/"+f)
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
    val lineRegex = """^===+$""".r
    blog.files.flatMap(globFiles).flatMap { f =>
      var ls = io.Source.fromFile(f).getLines.toVector
      val starts = (0 until ls.length).collect { case i if matches(lineRegex, ls(i)) => i-1 }
      (0 until starts.length).map { i =>
        parseArticle(ls.slice(starts(i), starts.lift(i+1).getOrElse(ls.length)))
      }
    }.toVector
  }






  def makeBase(implicit blog: Blog, markup: Markup): Base = {
    var articles: Vector[Article] = timer("readfiles")(readGallery(blog) ++ readPosts(blog))

    timer("checks") {
    if (blog.articlesMustNotBeMixed) {
      val (hidden, rest1) = articles.span { a => a.title.startsWith("?") }
      val (visible, rest) = rest1.span { a => !a.title.startsWith("?") }
      if (rest.nonEmpty) sys.error("hidden and visible articles are mixed up")
    }

    val hiddenSlugs: Set[Slug] = if (blog.dumpAll) Set() else {
      articles.filter(_.title.startsWith("?")).map(_.asSlug).toSet // this is done ahead of time beucase of article merging
    }

    for (a <- articles) {
      if (hiddenSlugs.contains(a.asSlug) && a.dates.nonEmpty && articles.count(_.slug == a.slug) > 1)
        sys.error(s"hidden and dated article are sharing the same slug '${a.slug}', this is most likely an error")
    }

    val now = new Date
    articles = articles.filter { a =>
      val isInPast = a.date == null || a.date.before(now)
      !hiddenSlugs.contains(a.asSlug) && isInPast
    }
    }

    timer("merge") {
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
        ) sys.error("two conflicting articles with the same slug '"+a.slug+"'")


        val merged = b.copy(
          dates   = a.dates ++ b.dates,
          tags    = a.tags merge b.tags,
          meta    = a.meta merge b.meta,
          license = if (a.license != null) a.license else b.license,
          rawText = if (a.rawText.length < b.rawText.length) a.rawText+"\n\n"+b.rawText else b.rawText+"\n\n"+a.rawText,
          images  = a.images ++ b.images,
          inFeed  = a.inFeed && b.inFeed
        ) // backlinks, similar, pub, pubBy not yet populated

        articleMap(a.slug) = merged
      }
    }

    articles = slugOrder.map(articleMap).toVector
    }

    // ordered by date
    timer("order by date") {
    if (blog.articlesMustBeSorted) {
      val dated = articles.filter(_.date != null)
      val ordered = dated == dated.sortBy(~_.date.getTime)
      if (!ordered) sys.error("articles are not ordered by date")
    }
    }

    var tagMap: Map[Tag, Seq[Article]] = timer("invert tags 1") { invert(articles.map { a => (a, (a.tags.visible).distinct) }) }

    // maps all article slugs (main ones and aliases) to their absolute urls
    val globalNames: Map[String, String] = timer("global names") {
      val allArticles: Seq[Article] = (Base(articles, tagMap).extraTags ++ articles)

      val slugs   = allArticles.map(_.slug)
      val aliases = allArticles.flatMap(_.aliases)

      for ((s, ss) <- slugs.groupBy(s => s)) if (ss.size > 1) sys.error(s"duplicate slug $s")
      //for ((a, as) <- aliases.groupBy(a => a)) if (as.size > 1) sys.error(s"duplicate alias $a") // TODO

      val slugSet = slugs.toSet
      for (a <- aliases) if (slugSet.contains(a)) sys.error(s"alias $a is colliding with a slug")

      allArticles.flatMap { a => (a.slug +: a.aliases).map(s => (s, blog.absUrl(a))) }.toMap
    }

    // globalMapping maps from slugs to absolute urls
    def resolveLink(link: String, localAliases: Map[String, String], globalMapping: Map[String, String], a: Article) = {
      val Array(base, hash) = link.split("#", 2).padTo(2, "")
      val Array(b, h) = localAliases.getOrElse(base, base).split("#", 2).padTo(2, "")
      if (blog.printErrors && b.nonEmpty && !isAbsolute(b) && !b.startsWith("#") && !b.startsWith("..") && !b.matches(".*\\.(php|jpg|png|gif|rss|zip|data|txt|scala|c)$") && !globalMapping.contains(b))
        println(s"bad link [$link -> $b] (in ${a.slug})")
      globalMapping.getOrElse(b, b)+(if (hash.nonEmpty) "#"+hash else "")+(if (h.nonEmpty) "#"+h else "")
    }

    timer("parse text") {
    articles = articles.map { a =>
      val txt = markup.process(a, (link, localAliases) => resolveLink(link, localAliases, globalNames, a), if (a.notes == null) "" else blog.absUrlFromSlug(a.notes), blog.imageRoot)

      // TODO linkAliases return map, no dupes can be detected
      txt.linkAliases.groupBy(_._1).filter(_._2.size > 1).foreach { case (l, _) =>
        sys.error(s"duplicate link refs [$l] in article '${a.slug}'")
      }
      txt.linkAliases.foreach { case (r, url) =>
        if (url.trim.startsWith("???")) {
          sys.error(s"undefined linkRef $r -> $url in article '${a.slug} (link prefixed by ??? in considered placeholder for missing url)'")
        }
      }

      // translate meta:rel specified by local alias
      val newMeta = if (a.rel.isEmpty) a.meta else {
        val localAliases = txt.linkAliases.toMap
        a.meta.withSeq("rel", a.meta.seq("rel").map { r => localAliases.getOrElse(r, r) })
      }

      a.copy(
        text = txt,
        meta = newMeta,
        // images might be already populated from readGallery()
        images = (a.images ++ txt.images)
      )
    }
    }


    val base = Base(articles, tagMap)
    val allTagMap: Map[Tag, Seq[Article]] = timer("invert tags 2") { invert(articles.map { a => (a, (a.tags.visible ++ a.tags.hidden).distinct) }) }
    val sim = new Similarities(base, allTagMap)
    def refs(a: Article, k: String) = a.meta.seq(k) flatMap base.find

    val backlinks: Map[Slug, Seq[Article]] = timer("backlinks") {
      invert(articles.map { a => (a, a.slugsOfLinkedArticles) })
        .map { case (k, as) => (k, as.distinct) }
    }

    val pubsBy: Map[Slug, Article] = timer("pubsBy") {
      invert(articles.map { a => (a, refs(a, "pub").map(_.asSlug)) })
        .map { case (k, vs) => (k, vs.sortBy(_.date).head) }
    }


    articles = timer("populate") { articles.par.map { a =>
      val bs = backlinks.getOrElse(a.asSlug, Seq())
      val pubBy = pubsBy.getOrElse(a.asSlug, null)

      a.meta.seq("pub") foreach { id =>
        if (!base.isValidId(id)) undefId(id, a)
      }

      a.copy(
        dates = if (a.dates.isEmpty && pubBy != null) pubBy.dates.take(1) else a.dates,
        backlinks = bs,
        similar = sim.similarByTags(a, count = blog.limitSimilar, without = bs),
        pub = refs(a, "pub"),
        pubBy = pubBy
      )
    }.seq }

    tagMap = invert(articles.map { a => (a, (a.tags.visible).distinct) }) // ??? recompute tag map

    if (blog.sortByDate) { // newest first, articles without date last
      val byDate = (a: Article) => ~(if (a.date == null) 0 else a.date.getTime)
      articles = articles.sortBy(byDate)
      tagMap = tagMap.map { case (t, as) => (t, as.sortBy(byDate)) }
    }

    tagMap = timer("final tagmap") { tagMap.map { case (t, as) =>
      val tagArticle = base.tagByTitle(t)
      val key = tagArticle.meta.scalar("sortby")

      if (key == null) { // sort by order linked in article (order for << >> navigation)
        val linked = tagArticle.slugsOfLinkedArticles.distinct
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
    }}

    Base(articles, tagMap)
  }




  def checkUrls(blog: Blog, base: Base) = ()
//    for {
//      a <- base.all ; l <- a.links
//      url = resolveLink(l, base, a) if !blog.isLocalLink(url)
//    } {
//      try {
//        val conn = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
//        conn.setRequestMethod("GET")
//        conn.connect()
//        val code = conn.getResponseCode
//        if (code >= 400) { println(code+" "+url) }
//      } catch {
//        case e: UnknownHostException => println("unknown host "+url)
//      }
//    }

  def tags(blog: Blog, base: Base) = {
    val titleLine = io.Source.stdin.getLines.take(1).toSeq.head
    val titleRegex(_, _, slug) = titleLine
    RecommendTags(blog, base, slug)
  }






  def makeFiles(blog: Blog, base: Base, markup: Markup) = {
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
      def mkDate(y: Int, m: Int) = Seq(new GregorianCalendar(y, m-1, 1).getTime)
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


    timer("generate and save files") {
    val archiveLinks = archivePages.par.map { case (a, as) =>
      val l = FlowLayout(blog.absUrl(a), base, blog, markup)
      val prev = archivePages((archivePages.indexWhere(_._1.slug == a.slug)-1) max 0 min (archivePages.length-1))._1
      val next = archivePages((archivePages.indexWhere(_._1.slug == a.slug)+1) max 0 min (archivePages.length-1))._1
      val body = l.makeIndex(Seq(), as, prev = prev, next = next)
      fileIndex ++= saveFile(blog.relUrl(a), l.makePage(body, containImages = false /* only links, no full articles */), oldFileIndex)
      a
    }.seq

    val path = blog.relUrlFromSlug("index")
    val l = FlowLayout(blog.absUrlFromPath(path), base, blog, markup)
    val body = l.makeIndex(fulls, links, archiveLinks, blog.groupArchiveBy == "month")
    fileIndex ++= saveFile(path, l.makePage(body, containImages = isIndexGallery), oldFileIndex)

    timer("generate and save files - articles") {
    base.articles.par foreach { a =>
      var l = FlowLayout(blog.absUrl(a), base, blog, markup)
      val body = l.makeFullArticle(a)
      fileIndex ++= saveFile(blog.relUrl(a), l.makePage(body, a.title, containImages = a.images.nonEmpty, headers = l.ogTags(a)), oldFileIndex)
    }
    }

    base.allTags.keys.par foreach { a =>
      var l = FlowLayout(blog.absUrl(a), base, blog, markup)
      val body = l.makeFullArticle(a)
      val hasImages = a.images.nonEmpty || base.allTags(a).exists(_.images.nonEmpty)
      fileIndex ++= saveFile(blog.relUrl(a), l.makePage(body, a.title, containImages = hasImages, headers = l.rssLink(a.slug+".xml")), oldFileIndex)
      fileIndex ++= saveXml(a.slug, makeRSS(base.allTags(a).take(blog.limitRss), null), oldFileIndex)
    }

    {
      val path = blog.relUrlFromSlug("tags")
      val l = FlowLayout(blog.absUrlFromPath(path), base, blog, markup)
      fileIndex ++= saveFile(path, l.makePage(l.makeTagIndex(base)), oldFileIndex)
    }

    def mkBody(a: Article) = {
      val body = FlowLayout(null, base, blog, markup).makeArticleBody(a, true)
      FlowLayout.updateLinks(body, url => blog.addParamMediumFeed(url))
    }
    fileIndex ++= saveXml("rss", makeRSS(base.feed.take(blog.limitRss), if (blog.articlesInRss) mkBody else null), oldFileIndex)

    if (blog.allowComments) {
      val l = FlowLayout(blog.absUrlFromPath("comments.php"), base, blog, markup)
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

    fileIndex ++= saveFile("out.php", outScript, oldFileIndex)

    if (blog.cssFile.nonEmpty) {
      fileIndex ++= saveFile("style.css", io.Source.fromFile(blog.cssFile).mkString, oldFileIndex)
    }

    fileIndex ++= saveFile("robots.txt", "User-agent: *\nAllow: /", oldFileIndex)
    saveFile(".files", fileIndex.toSeq.sorted.map { case (file, hash) => hash+" "+file }.mkString("\n"), oldFileIndex)
    }


    timer("resize images") {
    def smallThumbJob(img: Image) = (new File(blog.outDir, blog.thumbnailUrl(img)),           blog.thumbWidth,      blog.thumbHeight, 0.05f)
    def mainThumbJob(img: Image)  = (new File(blog.outDir, blog.bigThumbnailUrl(img, false)), blog.bigThumbWidth,   -1,               0.1f)
    def rightThumbJob(img: Image) = (new File(blog.outDir, blog.bigThumbnailUrl(img, true)),  blog.bigThumbWidth/2, -1,               0.1f)

    val resizeJobs = base.all.flatMap(_.images).map {
      case i if i.mods == "main" && i.align == ">" => (i, Seq(smallThumbJob(i), rightThumbJob(i)))
      case i if i.mods == "main"                   => (i, Seq(smallThumbJob(i), mainThumbJob(i)))
      case i                                       => (i, Seq(smallThumbJob(i)))
    }

    new File(blog.outDir, "t").mkdir()
    for ((image, jobs) <- resizeJobs) {
      try {
        lazy val file = {
          println(s"downloading ${image.url}")
          ImageIO.read(new URL(fixPath(image.url)))
        }
        for ((thumbFile, w, h, sharpenStrength) <- jobs if !thumbFile.exists) {
          println(s"resizing image ${image.url} -> $thumbFile")
            val suffix = image.url.split("\\.").last.toLowerCase
            val s = if (ImageIO.getWriterFileSuffixes.contains(suffix)) suffix else "jpg"
            val strength = if (s == "png" || s == "gif") 0f else sharpenStrength
            file match {
              case null => println(s"ImageIO.read(${image.url}) == null")
              case full => ImageIO.write(ImageTools.resizeImage(full, w, h, strength), s, thumbFile)
            }
        }
      } catch { case e: IIOException => println(e) }
    }
    }

  }

}
