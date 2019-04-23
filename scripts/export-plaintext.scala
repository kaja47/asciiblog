package asciiblog

object ExportPlaintext extends App {
  util.requireConfig(args)

  val (blog, base, _, _) = MakeFiles.init(args, b => b.copy(printErrors = false, printTiming = false))

  val stripTagRegex   = """\<.*?\>""".r
  val stripLinkRegex  = """:\[.+?\]""".r
  val refRegex        = """\[\[.+?\]\]""".r
  val whitespaceRegex = """(?u)\s+""".r
  val normalizeRegex  = """\s+[,.;]""".r

  def normalize(str: String) = {
    var t = str
    t = stripTagRegex.replaceAllIn(t, " ")
    t = stripLinkRegex.replaceAllIn(t, "")
    t = refRegex.replaceAllIn(t, "")
    t = whitespaceRegex.replaceAllIn(t, " ")
    t = normalizeRegex.replaceAllIn(t, "")
    t.toLowerCase.trim
  }

  def getText(sx: Seq[Segment]): String = sx.map {
    case t: Textual => t.txt
    case Block(tpe, txt, mods) =>
    case Blockquote(sx) => getText(sx)
    case SegmentSeq(sx) => getText(sx)
    case BulletList(items) => getText(items)
    case NumberedList(items) => getText(items.map(_._2))
    case Table(rows, _) => "" // TODO
    case _ => ""
  }.mkString(" ")

  base.all.foreach { a =>
    val txt = normalize(getText(a.text.asInstanceOf[AsciiText].segments))
    println(blog.relUrl(a)+" "+txt)
  }
}
