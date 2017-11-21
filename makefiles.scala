package asciiblog

import MakeFiles.{ Blog, hash, tagSlug, invert, undefId, isAbsolute, isLocalLink, extractSlug }
import java.io.{ File, FileWriter, FileOutputStream }
import java.net.{ URL, URI, HttpURLConnection, UnknownHostException }
import java.security.MessageDigest
import java.text.SimpleDateFormat
import java.util.{ Date, GregorianCalendar, Calendar, Locale, zip }
import javax.imageio.{ ImageIO, IIOException }
import scala.collection.{ mutable, immutable }
import scala.util.matching.Regex

case class Article(
  title: String,
  slug: String,
  dates: Seq[Date] = Seq(), // publishing date + dates of updates
  tags: Tags = Tags(),
  meta: Meta = Meta(),
  link: String = null,
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
  def slugsOfLinkedArticles = links.filter(isLocalLink).map(extractSlug)

  // image marker is shown only for images that are not from external sources
  def hasImageMarker = images.exists(i => i.source == null || i.source == "")
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
  override def toString = "Tag("+(if (supertag)"##"else"#")+title+")"
}


case class Base(all: Vector[Article], _tagMap: Map[Tag, Seq[Article]] = Map()) {
  private lazy val tagsOfImages: Seq[Tag] = all.flatMap(_.images).flatMap(_.tags.visible)
  lazy val tagMap: Map[Tag, Seq[Article]] = tagsOfImages.map(_ -> Seq()).toMap ++ _tagMap

  private lazy val imageTagMap: Map[Tag, Seq[(Image, Article)]] = invert(all.flatMap(a => a.images.map(i => ((i, a), i.tags.visible.distinct))))
  private def taggedImages(t: Tag) = imageTagMap.getOrElse(t, Seq()).map { case (i, a) => i.copy(inText = false, localSource = a) }

  lazy val extraTags: Seq[Article] = { // tags that have no particular article
    val direct = all.collect { case a if a.isTag => a.asTag }.toSet
    tagMap.collect { case (t, as) if !direct.contains(t) => Article(t.title, tagSlug(t.title), meta = Meta(Map((if (t.supertag) "supertag" else "tag") -> null)), text = AsciiText.empty) }.toSeq
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
  private val tm: Map[Tag, Array[Int]] = tagMap.map { case (t, as) => (t, as.map(artMap).toArray) }

  private case class Sim(article: Article, commonTags: Int)

  private def _similarByTags(a: Article): Seq[Sim] = {
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

    for (i <- a.rel.flatMap(base.find).flatMap(artMap.get)) {
      freq(i) += 64
    }

    (0 until arts.length).iterator.collect { case i if freq(i) >= 1 && arts(i).slug != a.slug =>
      Sim(arts(i), freq(i))
    } .toVector
      .sortBy { s => (~s.commonTags, dateDiff(a, s.article)) } // most common tags, published closest together
  }

  def similarByTags(a: Article, count: Int, without: Seq[Article]): Seq[Article] =
    _similarByTags(a).filter(s => !without.contains(s.article)).take(count).map(_.article)
}


object MakeFiles extends App {

  if (args.length < 1) {
    println("config file not specified")
    sys.exit()
  }

  private def kv: PartialFunction[String, (String, String)] = {
    case s if s.split(" ", 2).length == 2 =>
      val Array(k, v) = s.split(" ", 2); (k, v/*.replaceAll("""^"|"$""", "")*/)
  }

  private val thisDir = new File(".") // new File(System.getProperty("java.class.path")).getParent

  val galleryScript =
    io.Source.fromFile(thisDir+"/gallery.js").mkString
      .replaceAll("(?<!let|function|in)[\\s]+(?!in)|/\\*.*?\\*/", "") // rather crude and incorrect minifier

  val commentsScript =
    io.Source.fromFile(thisDir+"/comments.php").mkString

  val cfg = io.Source.fromFile(args(0)).getLines.collect(kv).toMap

  object Blog {
    val title: String          = cfg("title")
    val baseUrl: String        = cfg("baseUrl")
    val files: Seq[String]     = spaceSeparatedStrings(cfg.getOrElse("files", "").trim)
    val outDir: String         = cfg.getOrElse("outDir", null)
    val articlesOnIndex: Int   = cfg.getOrElse("fullArticlesOnIndex", "5").toInt
    val groupArchiveBy: String = cfg.getOrElse("groupArchiveBy", "year") // "year", "month" or some number
    val archiveFormat: String  = cfg.getOrElse("archiveFormat", "link").ensuring(f => f == "link" || f == "short")
    val tagFormat: String      = cfg.getOrElse("tagFormat", "link").ensuring(f => f == "link" || f == "short")
    val cssStyle: String       = cfg.getOrElse("style", "")
    val cssFile: String        = cfg.getOrElse("cssFile", "")
    val header: String         = cfg.getOrElse("header", "")
    val footer: String         = cfg.getOrElse("footer", "")
    val thumbWidth: Int        = cfg.getOrElse("thumbnailWidth", "150").toInt
    val thumbHeight: Int       = cfg.getOrElse("thumbnailHeight", "100").toInt
    val bigThumbWidth: Int     = cfg.getOrElse("bigThumbnailWidth", "800").toInt
    val limitRss: Int          = cfg.getOrElse("limitRss", Int.MaxValue.toString).toInt
    val articlesInRss: Boolean = cfg.getOrElse("fullArticlesInRss", "false").toBoolean
    val limitSimilar: Int      = cfg.getOrElse("limitSimilarLinks", "5").toInt
    val sortByDate: Boolean    = cfg.getOrElse("sortByDate", "false").toBoolean
    val imageRoot: String      = cfg.getOrElse("imageRoot", "")
    val articlesMustBeSorted: Boolean = cfg.getOrElse("articlesMustBeSorted", "false").toBoolean
    val articlesMustNotBeMixed: Boolean = cfg.getOrElse("articlesMustNotBeMixed", "false").toBoolean
    val language: String       = cfg.getOrElse("language", "en")
    val dumpAll: Boolean       = cfg.getOrElse("dumpAll", "false").toBoolean // ignore hidden articles, dump everything into main feed
    val compressFiles: Boolean = cfg.getOrElse("compressFiles", "false").toBoolean
    val fileSuffix: String     = cfg.getOrElse("fileSuffix", ".html")
    val imageMarker: String    = cfg.getOrElse("imageMarker", "")
    val albumsDir: String      = cfg.getOrElse("albumsDir", "")
    val allowComments: Boolean = cfg.getOrElse("allowComments", "false").toBoolean

    val openGraph: Boolean     = cfg.getOrElse("openGraph", "false").toBoolean
    val twitterSite: String    = cfg.getOrElse("twitter.site", "")
    val twitterCreator: String = cfg.getOrElse("twitter.creator", "")
    def hasOgTags = twitterSite.nonEmpty || twitterCreator.nonEmpty || openGraph

    val translation: Map[String, String] =
      io.Source.fromFile(thisDir+"/lang."+language).getLines.collect(kv).toMap
  }

  val markup = AsciiMarkup

  private def spaceSeparatedStrings(str: String): Seq[String] = str match {
    case "" => Seq()
    case str if str(0) == '"' =>
      val (s, rest) = str.drop(1).span(_ != '"')
      s +: spaceSeparatedStrings(rest.drop(1).trim)
    case _ =>
      val (s, rest) = str.span(_ != ' ')
      s +: spaceSeparatedStrings(rest.trim)
  }


  private val patternBracketRegex = """(?x) ^(.*?)\{(.*)\}$ """.r
  private def listFiles(pattern: String): Array[File] = ((pattern match {
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



  def invert[A, B](m: Seq[(A, Seq[B])]): Map[B, Seq[A]] =
    m.flatMap { case (a, bs) => bs.map { b => (b, a) }  }
      .groupBy(_._1)
      .map { case (b, bas) => (b, bas.map(_._2).toVector) }


  def absUrlFromPath(path: String) = Blog.baseUrl + "/" + path
  def absUrlFromSlug(slug: String) = Blog.baseUrl + "/" + slug + Blog.fileSuffix
  def relUrlFromSlug(slug: String) = slug + Blog.fileSuffix
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
  def extractSlug(url: String) = if (isLocalLink(url)) Slug(dropLocalPrefix(url).dropRight(Blog.fileSuffix.length)) else sys.error("not local url")
  def thumbnailUrl(img: Image) = s"t/${img.thumb}-${Blog.thumbWidth}x${Blog.thumbHeight}"+imgSuffix(img)
  def bigThumbnailUrl(img: Image, half: Boolean) = s"t/${img.thumb}-${Blog.bigThumbWidth / (if (half) 2 else 1)}"+imgSuffix(img)

  def imgSuffix(img: Image) = {
    val imageSuffixes = Set("jpg", "jpeg", "png")
    val suffix = img.url.split("\\.").last.toLowerCase
    if (imageSuffixes.contains(suffix)) "."+suffix else ""
  }

  def undefId(id: String, a: Article) = {
    println(s"id [$id] is not defined in article '${a.title}'")
    "undefined id "+id
  }


  private val titleRegex    = """^(XXX+\s*)?(.+?)(?:\[([^ ]+)\])?$""".r
  private val dateRegex     = """^(\d+)-(\d+)-(\d+)(?: (\d+):(\d+)(?::(\d+))?)?""".r
  val licenses = Set("CC by", "CC by-nc", "CC by-nd", "CC by-sa", "CC by-nc-nd", "CC by-nc-sa")

  def parseDates(l: String): Option[Seq[Date]] = {
    val dates = l.split(",").map(l => parseDate(l.trim))
    if (dates.nonEmpty && dates.forall(_.isDefined)) Some(dates.map(_.get)) else None
  }

  def parseDate(l: String) = l match {
    case dateRegex(y, m, d, null, null, null) =>
      Some(new GregorianCalendar(y.toInt, m.toInt-1, d.toInt).getTime)
    case dateRegex(y, m, d, h, mi, null) =>
      Some(new GregorianCalendar(y.toInt, m.toInt-1, d.toInt, h.toInt, mi.toInt, 0).getTime)
    case dateRegex(y, m, d, h, mi, s) =>
      Some(new GregorianCalendar(y.toInt, m.toInt-1, d.toInt, h.toInt, mi.toInt, s.toInt).getTime)
    case _ => None
  }

  def parseLicense(l: String): Option[String] =
    if (licenses.contains(l)) Some(l) else None

  val bracketRegex  = """^\(([^)]+)\)$""".r
  def bracketed(xs: Array[String])   = xs.collect { case bracketRegex(x) => x }
  def unbracketed(xs: Array[String]) = xs.filter { case bracketRegex(x) => false ; case _ => true }

  val tagsRegex     = """^((?:#).+)$""".r
  val tagBlockRegex = """(##|#)(.+?)(?=( |^)(##|#)|$)""".r
  def getTags(s: String) =
    tagBlockRegex.findAllMatchIn(s).foldLeft(Tags()) { (t, m) =>
      val tags = m.group(2).split("\\s*,\\s*").map(_.trim)
      val (vis, hid): (Seq[Tag], Seq[Tag]) = (m.group(1) match {
        case "##" => (tags.map(Tag(_, true)), Seq())
        case "#"  => (unbracketed(tags).map(Tag(_)), bracketed(tags).map(Tag(_)))
      })
      t.copy(visible = t.visible ++ vis, hidden = t.hidden ++ hid)
    }
  def peelOffTags(s: String): (String, Tags) = (tagBlockRegex.replaceAllIn(s, "").trim, getTags(s))
  val parseTags: PartialFunction[String, Tags] = { case tagsRegex(s) => getTags(s) }

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

  def parseLink(l: String): Option[String] =
    if (l.startsWith("link:")) {
      Some(l.drop("link:".length).trim)
    } else None

  def hash(txt: String): String = hash(txt.getBytes("utf-8"))
  def hash(txt: Array[Byte]): String = BigInt(1, _md5(txt)).toString(16).reverse.padTo(32, '0').reverse
  def _md5(bytes: Array[Byte]) = MessageDigest.getInstance("MD5").digest(bytes)

  def parseArticle(lines: Vector[String]): Article = {
    val ls = lines.map(_.replaceAll("\\s*$", ""))

    val titleLine = ls(0)
    val titleRegex(xxx, title, slug) = titleLine

    val (metaLines, _body) = ls.drop(2).span(l => l.nonEmpty)
    val body = _body.dropWhile(l => l.isEmpty).reverse.dropWhile(l => l.isEmpty).reverse

    val dates   = metaLines.flatMap(parseDates _     )
    val tags    = metaLines.flatMap(parseTags.lift   andThen (_.toSeq))
    val license = metaLines.flatMap(parseLicense _   andThen (_.toSeq))
    val links   = metaLines.flatMap(parseLink _      andThen (_.toSeq))
    val metas   = metaLines.flatMap(l => parseMeta(l, "meta: ").toSeq)
    val meta = metas.foldLeft(Meta())(_ merge _)

    val isTag = meta.values.contains("supertag") || meta.values.contains("tag") || (slug != null && isTagSlug(slug))
    val inFeed = Blog.dumpAll || (xxx == null && !isTag)
    val realSlug =
      if (slug != null && slug != "") slug else
        if (isTag) tagSlug(title)
        else generateSlug(title)

    if ((dates ++ tags ++ license ++ links ++ metas).size < metaLines.size)
      sys.error("some metainformation was not processed: "+metaLines)

    links.foreach(l => require(isAbsolute(l), s"urls in link: field must be absolute ($realSlug)"))

    new Article(
      title   = title.trim,
      slug    = realSlug,
      dates   = dates.flatten,
      tags    = tags.fold(Tags()) { (a, b) => a.merge(b) },
      meta    = meta,
      link    = links.headOption.getOrElse(null),
      license = license.headOption.getOrElse(null),
      rawText = body.mkString("\n"),
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



  def saveFile(f: String, content: String): Seq[(String, String)] = { // filename -> hash
    val g = f+".gz"
    val ff = new File(Blog.outDir, f)
    val gf = new File(Blog.outDir, g)

    val p = ff.getParentFile
    if (p != null) p.mkdirs()

    val fw = new FileWriter(ff)
    fw.write(content)
    fw.close()

    val h = hash(content)

    Seq(f -> h) ++ (if (!Blog.compressFiles) Seq() else {
      val out = new zip.GZIPOutputStream(new FileOutputStream(gf))
      out.write(content.getBytes("utf-8"))
      out.close()
      Seq(g -> h)
    })
  }

  def saveXml(f: String, content: String): Seq[(String, String)] =
    saveFile(f+".xml", content)



  def readGallery(): Vector[Article] = {
    if (Blog.albumsDir.isEmpty) return Vector()

    val albumDirs = new File(Blog.albumsDir, "albums").listFiles.sortBy(_.getName).reverse.toSeq
    val dateTitle = """^(?:(\d+)-(\d+)-(\d+)\s*-?\s*)?(.*)$""".r

    for (albumDir <- albumDirs.toVector) yield {
      println(albumDir.getName)

      val dateTitle(y, m, d, t) = albumDir.getName
      val (date, title) =
        if (y == null) (null, t)
        else (new GregorianCalendar(y.toInt, m.toInt-1, d.toInt).getTime, t)

      val imgUrls = albumDir.list collect { case f if f.toLowerCase.endsWith(".jpg") =>
        Blog.baseUrl+"/albums/"+albumDir.getName+"/"+f
      }

      new Article(
        title  = if (title.nonEmpty) title else albumDir.getName,
        slug   = generateSlug(albumDir.getName),
        dates  = Seq(date),
        images = imgUrls.map { url => new Image(url, inText = false) }
      )
    }
  }

  def readPosts(): Vector[Article] =
    Blog.files.flatMap(listFiles).flatMap { f =>
      var ls = io.Source.fromFile(f).getLines.toVector
      val starts = ls.zipWithIndex.collect { case (l, i) if l.matches("===+") => i-1 }
      (0 until starts.length).map { i =>
        parseArticle(ls.slice(starts(i), starts.lift(i+1).getOrElse(ls.length)))
      }
    }.toVector


  def timer[T](label: String)(f: => T) = {
    val s = System.nanoTime
    val r = f
    val d = System.nanoTime - s
    println(label+" "+(d/1e6)+"ms")
    r
  }


  val base: Base = {
    var articles: Vector[Article] = timer("readfiles")(readGallery() ++ readPosts())

    timer("checks") {
    if (Blog.articlesMustNotBeMixed) {
      val (hidden, rest1) = articles.span { a => a.title.startsWith("?") }
      val (visible, rest) = rest1.span { a => !a.title.startsWith("?") }
      if (rest.nonEmpty) sys.error("hidden and visible articles are mixed up")
    }

    val hiddenSlugs: Set[Slug] = if (Blog.dumpAll) Set() else {
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
          //(a.rawText.nonEmpty && b.rawText.nonEmpty)
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
    if (Blog.articlesMustBeSorted) {
      val dated = articles.filter(_.date != null)
      val ordered = dated == dated.sortBy(~_.date.getTime)
      if (!ordered) sys.error("articles are not ordered by date")
    }
    }

    var tagMap   : Map[Tag, Seq[Article]] = timer("invert tags 1") { invert(articles.map { a => (a, (a.tags.visible).distinct) }) }
    val allTagMap: Map[Tag, Seq[Article]] = timer("invert tags 2") { invert(articles.map { a => (a, (a.tags.visible ++ a.tags.hidden).distinct) }) }

    // maps all article slugs (main ones and aliases) to their absolute urls
    val globalNames: Map[String, String] = timer("global names") {
      (Base(articles, tagMap).extraTags ++ articles)
        .flatMap { a => (a.slug +: a.aliases).map(s => (s, absUrl(a))) }.toMap
    }

    // globalMapping maps from slugs to absolute urls
    def resolveLink(link: String, localAliases: Map[String, String], globalMapping: Map[String, String], a: Article) = {
      val l = localAliases.getOrElse(link, link)
      //if (!isAbsolute(l) && !l.startsWith("#") && !l.startsWith("..") && !l.matches(".*\\.(php|jpg|png|gif|rss)$") && !globalMapping.contains(l)) println(s"bad link [$link -> $l] (in ${a.slug})")
      globalMapping.getOrElse(l, l)
    }

    timer("parse text") {
    articles = articles.map { a =>
      val txt = markup.process(a.rawText, (link, localAliases) => resolveLink(link, localAliases, globalNames, a))

      txt.linkAliases.groupBy(_._1).filter(_._2.size > 1).foreach { case (l, _) =>
        sys.error(s"duplicate link refs [$l] in article '${a.slug}'")
      }
      txt.linkAliases.foreach { case (r, url) =>
        if (url.trim.startsWith("???")) {
          sys.error(s"undefined linkRef $r -> $url in article '${a.slug} (link prefixed by ??? in considered placeholder for missing url)'")
        }
      }

      a.copy(
        text = txt,
        images = a.images ++ txt.images // images might be already populated from readGallery()
      )
    }
    }


    val base = Base(articles, tagMap)
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


    articles = articles map { a =>
      val bs = backlinks.getOrElse(a.asSlug, Seq())
      val pubBy = pubsBy.getOrElse(a.asSlug, null)

      a.meta.seq("pub") foreach { id =>
        if (!base.isValidId(id)) undefId(id, a)
      }

      a.copy(
        dates = if (a.dates.isEmpty && pubBy != null) pubBy.dates.take(1) else a.dates,
        backlinks = bs,
        similar = sim.similarByTags(a, count = Blog.limitSimilar, without = bs),
        pub = refs(a, "pub"),
        pubBy = pubBy
      )
    }

    tagMap = invert(articles.map { a => (a, (a.tags.visible).distinct) }) // ??? recompute tag map

    if (Blog.sortByDate) { // newest first, articles without date last
      val byDate = (a: Article) => ~(if (a.date == null) 0 else a.date.getTime)
      articles = articles.sortBy(byDate)
      tagMap = tagMap.map { case (t, as) => (t, as.sortBy(byDate)) }
    }

    tagMap = tagMap.map { case (t, as) =>
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
    }

    Base(articles, tagMap)
  }



//  if (args.length == 2 && args(1) == "checkLinks") {
//    for {
//      a <- base.all ; l <- a.links
//      url = resolveLink(l, base, a) if !isLocalLink(url)
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
//    sys.exit()
//  }


  val fileIndex = collection.concurrent.TrieMap[String, String]()
  val isIndexGallery = base.feed.take(Blog.articlesOnIndex).exists(_.images.nonEmpty)

  val (fulls, rest) = base.feed.splitAt(Blog.articlesOnIndex)

  def chunk[T: Ordering](as: Vector[Article])(g: Date => T)(zero: T)(f: T => Article): Vector[(Article, Vector[Article])] =
    as.groupBy { a => if (a.date == null) zero else g(a.date) }.toVector.sortBy(_._1).reverse.map { case (t, as) => (f(t), as) }

  val (links, archivePages) = timer("group archive") {
    def mkDate(y: Int, m: Int) = Seq(new GregorianCalendar(y, m-1, 1).getTime)
    val title = Blog.translation("archive")
    (Blog.groupArchiveBy match {
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
    val l = FlowLayout(absUrl(a), base, Blog, markup)
    val prev = archivePages((archivePages.indexWhere(_._1.slug == a.slug)-1) max 0 min (archivePages.length-1))._1
    val next = archivePages((archivePages.indexWhere(_._1.slug == a.slug)+1) max 0 min (archivePages.length-1))._1
    val body = l.makeIndex(Seq(), as, prev = prev, next = next)
    fileIndex ++= saveFile(relUrl(a), l.makePage(body, containImages = false /* only links, no full articles */))
    a
  }.seq

  val path = relUrlFromSlug("index")
  val l = FlowLayout(absUrlFromPath(path), base, Blog, markup)
  val body = l.makeIndex(fulls, links, archiveLinks, Blog.groupArchiveBy == "month")
  fileIndex ++= saveFile(path, l.makePage(body, containImages = isIndexGallery))

  base.articles.par foreach { a =>
    var l = FlowLayout(absUrl(a), base, Blog, markup)
    val body = l.makeFullArticle(a)
    fileIndex ++= saveFile(relUrl(a), l.makePage(body, a.title, containImages = a.images.nonEmpty, headers = l.ogTags(a)))
  }

  base.allTags.keys.par foreach { a =>
    var l = FlowLayout(absUrl(a), base, Blog, markup)
    val body = l.makeFullArticle(a)
    fileIndex ++= saveFile(relUrl(a), l.makePage(body, a.title, containImages = base.allTags(a).exists(_.images.nonEmpty), headers = l.rssLink(a.slug+".xml")))
    fileIndex ++= saveXml(a.slug, makeRSS(base.allTags(a).take(Blog.limitRss), null))
  }

  {
    val path = relUrlFromSlug("tags")
    val l = FlowLayout(absUrlFromPath(path), base, Blog, markup)
    fileIndex ++= saveFile(path, l.makePage(l.makeTagIndex(base)))
  }

  def mkBody(a: Article) = FlowLayout(null, base, Blog, markup).makeArticleBody(a, true)
  fileIndex ++= saveXml("rss", makeRSS(base.feed.take(Blog.limitRss), if (Blog.articlesInRss) mkBody else null))

  if (Blog.allowComments) {
    val l = FlowLayout(absUrlFromPath("comments.php"), base, Blog, markup)
    val p = l.makePage("{comments.body}", null, false,  null, includeCompleteStyle = true)
    val Array(pre, post) = p.split(Regex.quote("{comments.body}"))

    fileIndex ++= saveFile("comments.php", {
      val replaces = Blog.translation.collect { case (k, v) if k.startsWith("comments.") => s"{$k}" -> v } ++ Seq(
        "{comments.prebody}"  -> pre,
        "{comments.postbody}" -> post,
        "{comments.baseUrl}"  -> Blog.baseUrl,
        """href="rss.xml""""  -> """href="'.escapeHtmlAttr($requestUrl).'&amp;rss"""" // this is a bit ugly trick
      )
      var cs = commentsScript
      for ((from, to) <- replaces) {
        cs = cs.replace(from, to)
      }
      cs
    })
    new File(".comments").mkdirs()
    fileIndex ++= saveFile(".comments/.htaccess", "Deny from all")
  }

  if (Blog.cssFile.nonEmpty) {
    fileIndex ++= saveFile("style.css", io.Source.fromFile(Blog.cssFile).mkString)
  }

  fileIndex ++= saveFile("robots.txt", "User-agent: *\nAllow: /")
  saveFile(".files", fileIndex.toSeq.sorted.map { case (file, hash) => hash+" "+file }.mkString("\n"))
  }


  timer("resize images") {
  def smallThumbJob(img: Image) = (img, new File(Blog.outDir, thumbnailUrl(img)), Blog.thumbWidth, Blog.thumbHeight)
  def mainThumbJob(img: Image)  = (img, new File(Blog.outDir, bigThumbnailUrl(img, false)), Blog.bigThumbWidth, -1)
  def rightThumbJob(img: Image) = (img, new File(Blog.outDir, bigThumbnailUrl(img, true)), Blog.bigThumbWidth/2, -1)

  val resizeJobs = base.all.flatMap(_.images).flatMap {
    case i if i.mods == "main" && i.align == ">" => Seq(smallThumbJob(i), rightThumbJob(i))
    case i if i.mods == "main"                   => Seq(smallThumbJob(i), mainThumbJob(i))
    case i                                       => Seq(smallThumbJob(i))
  }

  new File(Blog.outDir, "t").mkdir()
  for ((image, thumbFile, w, h) <- resizeJobs if !thumbFile.exists) {
    println(s"resizing image ${image.url} -> $thumbFile")
    try {
      val suffix = image.url.split("\\.").last.toLowerCase
      val s = if (ImageIO.getWriterFileSuffixes.contains(suffix)) suffix else "jpg"
      ImageIO.read(new URL(fixPath(image.url))) match {
        case null => println(s"ImageIO.read(${image.url}) == null")
        case full => ImageIO.write(ImageTools.resizeImage(full, w, h), s, thumbFile)
      }
    } catch { case e: IIOException => println(e) }
  }
  }

}
