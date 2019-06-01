package asciiblog

import java.lang.StringBuilder
import scala.util.matching.Regex
import Colors._
import ProtoHighlighter._

object Highlighter {
  val highlighters: Map[String, Highlighter] = Map(
    "scala"      -> new ScalaHighlighter,
    "java"       -> new JavaHighlighter,
    "php"        -> new PHPHighlighter,
    "javascript" -> new JSHighlighter,
    "c"          -> new CHighlighter
  )

  def highlight(source: String, lang: String): String =
    highlighters.get(lang) match {
      case Some(hl) => applyHighlights(source, hl(source))
      case None     => source
    }

  def applyHighlights(source: String, lights: Seq[Light]): String = {
    val sb = new StringBuilder
    var lastEnd = 0
    for (l <- lights) {
      sb.append(util.escape(source.substring(lastEnd, l.start)))
      sb.append("<span class=hl"+l.color+">")
      sb.append(util.escape(source.substring(l.start, l.start+l.length)))
      sb.append("</span>")
      lastEnd = l.start+l.length
    }
    sb.append(util.escape(source.substring(lastEnd)))
    sb.toString
  }
}


case class Light(start: Int, length: Int, color: Int)


trait Highlighter {
  def apply(source: String): Seq[Light]
}


object Colors {
  // colors in molokai vim theme 
  val Red    = 0
  val Green  = 1
  val Blue   = 2
  val Purple = 3
  val Yellow = 4
  val Gray   = 5
  val Orange = 6
}


// `regexes` parametr must not contain parenthesized capturing groups
class ProtoHighlighter(regexes: Seq[(Int, String)]) extends Highlighter {
  private val (colors, rgxs) = regexes.unzip
  private val regex = rgxs.map(r => "("+r.trim+")").mkString("|").r

  def apply(source: String): Seq[Light] =
    regex.findAllMatchIn(source).map { m =>
      val g = (1 to m.groupCount).indexWhere(i => m.group(i) != null)
      Light(m.start, m.end-m.start, colors(g))
    }.toVector
}

object ProtoHighlighter {
  def mkTokenRegex(t: String): String =
    (if (t.head.isLetter) "\\b" else "") + quote(t) + (if (t.last.isLetter) "\\b" else "")

  private def quote(r: String) = if (r.matches("^[a-zA-Z0-9]+$")) r else Regex.quote(r)

  def tokens(ts: String) = ts.split(" +").map(mkTokenRegex).mkString("|")

  val comments = Seq(
    Gray   -> """(?xs) /\* .* \*/ """,
    Gray   -> """(?xm) //.*$ """
  )

  val numbers = Seq(
    Purple -> """(?x)  -? \b\d+ (?:\.\d+)? """
  )
}


class ScalaHighlighter extends ProtoHighlighter(Seq(
  Red    -> tokens("class trait object extends with var val def if else match case while do for type forSome try catch finally yield macro return"),
  Green  -> tokens("null true false ne eq this super new override final private protected implicit import package sealed <- -> => require"),
  Green  -> """(?x)  @\w+ """,                 // annotation
  Blue   -> """(?x)  \b[A-Z][\w$]*\b """,      // type
  Purple -> """(?x)  \b0x[a-fA-F0-9]+[lL]? """,// hex number
  Purple -> """(?x)  -? \b\d+ (?:\.\d+)? """,  // number
  Yellow -> """(?xs) \"\"\" .*? \"\"\" """,    // string
  Yellow -> """(?x)  " (?:\\"|[^"])* "   """,  // string
  Yellow -> """(?x)  ' (?:\\'|.) ' """,        // char
  Green  -> """(?x)  (?<=(?:val|var|def)\s+) [a-z][\w$]* """ // declaration
) ++ comments)


class JavaHighlighter extends ProtoHighlighter(Seq(
  Red    -> tokens("class interface extends implements var if else case whitch while do for try catch finally return"),
  Green  -> tokens("null true false this super new final private protected import package"),
  Blue   -> tokens("int long double float boolean void byte char short"),
  Green  -> """(?x)  @\w+ """,                 // annotation
  Blue   -> """(?x)  \b[A-Z]\w*\b """,         // type
  Purple -> """(?x)  \b0x[a-fA-F0-9]+[lL]? """,// hex number
  Purple -> """(?x)  -? \b\d+ (?:\.\d+)? """,  // number
  Yellow -> """(?x)  " (?:\\"|[^"])* "   """,  // string
  Yellow -> """(?x)  ' (?:\\'|.) ' """,        // char
) ++ comments)


class PHPHighlighter extends ProtoHighlighter(comments ++ Seq(
  Gray   -> tokens("<?php"),
  Red    -> tokens("class for if else elsif declare while foreach as function return yield from echo die isset unset <=> -> => $ !== != === == ... << >> < > <= >= ++ -- ** + - * / % ~ . ! && || & ^ | ?? ?:"),
  Purple -> tokens("true false null new clone instanceof"),
  Blue   -> tokens("private protected var const (bool) (float) (double) (string) (int) (void)"),
  Green  -> tokens("namespace use"),
  Orange -> """(?x)  (?<=\$) \w+ """,         // variable
  Purple -> """(?x)  -? \b\d+ (?:\.\d+)? """, // number
  Yellow -> """(?x)  " (?:\\"|[^"])* "  """,  // string
  Yellow -> """(?x)  ' (?:\\'|[^'])* '  """   // string
))


class JSHighlighter extends ProtoHighlighter(Seq(
  Red    -> tokens("for while do if return let var const document function"),
  Green  -> tokens("null true false undefined new this =>"),
  Purple -> """(?x)  -? \b\d+ (?:\.\d+)? """, // number
  Yellow -> """(?x)  " (?:\\"|[^"])* "  """,  // string
  Yellow -> """(?x)  ' (?:\\'|[^'])* '  """   // string
) ++ comments)


class CHighlighter extends ProtoHighlighter(Seq(
  Red    -> tokens("for return switch break continue"),
  Blue   -> tokens("int long short char void signed unsigned float double size_t struct"),
  Orange -> tokens("const"),
  Yellow -> tokens("case"),
  Purple -> """(?x)  -? \b\d+ (?:\.\d+)? """, // number
  Yellow -> """(?x)  " (?:\\"|[^"])* "  """,  // string
  Yellow -> """(?x)  ' (?:\\'|[^'])* '  """   // string
) ++ comments)
