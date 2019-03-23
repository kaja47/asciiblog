package asciiblog.example

import asciiblog._
import util.localDateTimeOrdering
import java.time.format.DateTimeFormatter

class CustomHooks extends NoHooks {

  val dateFormat = DateTimeFormatter.ofPattern("d. M. yyyy")

  private def plusArticles(drop: Int, base: Base) = {
    def plus(a: Article): Int = { val p = a.meta.value("+"); if (p == null) 0 else p.toInt }
    base.feed.slice(drop, 60+drop).filter(a => plus(a) > 0).sortBy{ a => (plus(a), a.date) }.reverse.take(14)
  }

  def makeHighlights(blog: Blog, base: Base, layout: Layout) = {
    val selection = plusArticles(7, base)
    val html = (if (selection.nonEmpty) {
      "<style>@media screen and (max-width: 800px){ .side {display:none} }</style>"+
      "<div class=side style='padding:2em; font-size:0.85em'>"+
      "<div><b>"+blog.translation.getOrElse("dontOverlook", "nepřehlédněte")+"</b></div>"+
      selection.map { a =>
        "<span>"+layout.articleLink(a, a.title, true, false)+"</span> "
      }.mkString+
      "</div>"
    } else "")
    PagePart.Text(html)
  }

  override def makePagePart(base: Base, blog: Blog, layout: Layout, name: String, args: String): PagePart = name match {
    case "highlights" => makeHighlights(blog, base, layout)
  }

  // nicer title
  override def title(base: Base, blog: Blog, layout: Layout, article: Article, compact: Boolean): String = {
    val formattedDate = if (article.date != null) dateFormat.format(article.date) else ""
    val pubByLink = if (!compact && article.pubBy != null) layout.articleLink(article.pubBy, formattedDate, true, false) else formattedDate

    "<h2>"+layout.articleLink(article, article.title, compact, false)+"</h2>"+
	  "<div style='color: #777;font-size:0.9em'>"+
	  pubByLink+
    (if (!article.isTag && article.author != null) " — "+article.author else "")+
	  "</div>"
  }

}
