package asciiblog

trait Hooks {
  def fullArticleBottom(base: Base, blog: Blog, layout: Layout, article: Article): String
  def title(base: Base, blog: Blog, layout: Layout, article: Article, compact: Boolean): String
  def listTitle(base: Base, blog: Blog, layout: Layout, article: Article): String
  def list(base: Base, blog: Blog, layout: Layout, articles: Seq[Article]): String
  def header(base: Base, blog: Blog, layout: Layout, article: Option[Article]): String
  def makePagePart(base: Base, blog: Blog, layout: Layout, name: String, args: String): PagePart

  /** This hook is called after all articles are parsed, but before any processing and checks on them. */
  def prepareArticles(blog: Blog, articles: Seq[Article]): Seq[Article]
  /** This hook is called after articles are processed and the final Base is computed. */
  def updateBase(base: Base, blog: Blog): Base
  /** This hook is called after blog is ganerated and cannot affect result in any way. */
  def afterGenerate(base: Base, blog: Blog): Unit
}

class NoHooks extends Hooks {
  def fullArticleBottom(base: Base, blog: Blog, layout: Layout, article: Article): String = ""
  def title(base: Base, blog: Blog, layout: Layout, article: Article, compact: Boolean): String = null
  def listTitle(base: Base, blog: Blog, layout: Layout, article: Article): String = null
  def list(base: Base, blog: Blog, layout: Layout, articles: Seq[Article]): String = null
  def header(base: Base, blog: Blog, layout: Layout, article: Option[Article]): String = null
  def makePagePart(base: Base, blog: Blog, layout: Layout, name: String, args: String): PagePart = sys.error(s"unknown part '$name'")

  def prepareArticles(blog: Blog, articles: Seq[Article]): Seq[Article] = articles
  def updateBase(base: Base, blog: Blog): Base = base
  def afterGenerate(base: Base, blog: Blog): Unit = ()
}

class LispyHooks(scriptFile: String) extends Hooks {
  import Lispy._

  val srcCode = io.Source.fromFile(scriptFile).mkString
  val (_, resEnv) = evalMain(parse(srcCode), env)

  def call[T](method: String, args: List[Any]): T =
    (resEnv.get(method) match {
      case Some(f: Func) => f(args.toList)
      case Some(Str(s)) => s
      case Some(x) => sys.error(s"hook $method must be function or string, $x given")
      case None =>
        method match {
          case "@index-prepend" | "@after-first-article" | "@full-article-bottom" => ""
          case "@prepare-articles" | "@update-base"  => args.head
          case _ => null
        }
    }).asInstanceOf[T]

  def fullArticleBottom(base: Base, blog: Blog, layout: Layout, article: Article): String =
    call[String]("@full-article-bottom", List(base, blog, layout, article))
  def title(base: Base, blog: Blog, layout: Layout, article: Article, compact: Boolean): String =
    call[String]("@title", List(base, blog, layout, article, compact))
  def listTitle(base: Base, blog: Blog, layout: Layout, article: Article): String =
    call[String]("@list-title", List(base, blog, layout, article))
  def list(base: Base, blog: Blog, layout: Layout, articles: Seq[Article]): String =
    call[String]("@list", List(base, blog, layout, articles))
  def header(base: Base, blog: Blog, layout: Layout, article: Option[Article]): String =
    call[String]("@header", List(base, blog, layout, article))
  def makePagePart(base: Base, blog: Blog, layout: Layout, name: String, args: String): PagePart =
    ???
  def prepareArticles(blog: Blog, articles: Seq[Article]): Seq[Article] =
    call[Seq[Article]]("@prepare-articles", List(blog, articles))
  def updateBase(base: Base, blog: Blog): Base =
    call[Base]("@update-base", List(base, blog))
  def afterGenerate(base: Base, blog: Blog): Unit =
    call[Unit]("@after-generate", List(base, blog))
}
