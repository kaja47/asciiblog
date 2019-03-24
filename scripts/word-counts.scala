package asciiblog

object WordCounts extends App {

  if (args.length < 1) {
    println("config file not specified")
    sys.exit()
  }

  val (_, base, _, _) = MakeFiles.init(args, _.copy(printErrors = false, printTiming = false))

  def splitWords(a: Article) = a.rawText.mkString(" ").split("\\s+")

  def stats(articles: Seq[Article]) = {
    val count = articles.size
    val wc    = articles.flatMap(splitWords).size
    f"$count%3d articles, $wc%6d words"
  }

  base.all
    .filter { _.date != null }
    .groupBy(a => a.date.getYear)
    .toSeq
    .sortBy { case (y, _) => y }
    .foreach { case (y, as) => println(y+": "+stats(as)) }

  val totalWords = base.all.map(a => splitWords(a).size).sum
  println(s"total words $totalWords")
}
