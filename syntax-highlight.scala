package asciiblog

import java.lang.StringBuilder
import scala.collection.mutable
import scala.util.matching.Regex

object Highlighter {
  val highlighters: Map[String, Highlighter] = Map(
    "scala"      -> new ScalaHighlighter,
    "php"        -> new PHPHighlighter,
    "javascript" -> new JSHighlighter,
    "c"          -> new CHighlighter
  )

  def highlight(source: String, lang: String): String =  {
    highlighters.get(lang) match {
      case Some(hl) => applyHighlights(source, hl(source))
      case None     => source
    }
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


trait ProtoHighlighter extends Highlighter {
  def apply(source: String): Seq[Light] =
    regex.findAllMatchIn(source).map { m =>
      Light(m.start, m.end-m.start, _colorFor(m.matched))
    }.toVector

  def tokens: Seq[(Int, Seq[String])]

  def mkTokenRegex(t: String): String =
    (if (t.head.isLetter) "\\b" else "") + scala.util.matching.Regex.quote(t) + (if (t.last.isLetter) "\\b" else "")

  def regexes: Seq[String]

  def colorFor(str: String): Int

  lazy val tokenColors: Map[String, Int] =
    tokens.flatMap { case (c, ts) => ts.map(t => (t, c)) }.toMap

  def _colorFor(str: String): Int =
    tokenColors.get(str).getOrElse(colorFor(str))

  lazy val regex = ((regexes :+ tokens.flatMap(_._2).map(mkTokenRegex).mkString("|")): Seq[String])
    .map(r => "("+r.trim+")")
    .mkString("|").r

  // colors in molokai vim theme 
  val Red    = 0
  val Green  = 1
  val Blue   = 2
  val Purple = 3
  val Yellow = 4
  val Gray   = 5
  val Orange = 6
}

class ScalaHighlighter extends ProtoHighlighter {

  val tokens = Seq(
    Red   -> "class trait object extends with var val def if else match case while do for type forSome try catch finally yield macro".split(" "),
    Green -> "null true false ne eq this super new override final private protected implicit import package sealed <- -> => require".split(" ")
  )

  val regexes = Seq(
    """(?x)  @\w+ """,              // annotation
    """(?x)  \b[A-Z]\w*\b """,      // type
    """(?x)  -? \b\d+(\.\d+)? """,  // number
    """(?xs) \"\"\" .*? \"\"\" """, // string
    """(?x)  " (\\"|[^"])* "   """, // string
    """(?x)  ' (\\'|.) ' """,       // char
    """(?xs) /\* .* \*/ """,        // comment
    """(?xm) //.*$ """,             // comment
    """(?x)  (?<=(val|var|def)\s+) [a-z]\w* """ // declaration
  )

  def colorFor(str: String) = str match {
    case str if str(0) == '@'   => Green
    case str if str(0).isUpper  => Blue
    case str if str(0).isDigit  => Purple
    case str if str(0) == '-'   => Purple
    case str if str(0) == '"'   => Yellow
    case str if str(0) == '\''  => Yellow
    case str if str(0) == '/'   => Gray
    case str if str(0).isLetter => Green
  }
}

class PHPHighlighter extends ProtoHighlighter {

  val tokens = Seq(
    Gray   -> "<?php".split(" "),
    Red    -> "class for if declare while foreach as function return yield from echo die isset unset <=> -> => $ !== != === == ... << >> < > <= >= ++ -- ** + - * / % ~ . ! && || & ^ | ?? ?:".split(" "),
    Purple -> "true false null new clone instanceof".split(" "),
    Blue   -> "private protected var const (bool) (float) (double) (string) (int) (void)".split(" "),
    Green  -> "namespace use".split(" ")
  )

  val regexes = Seq(
    """(?x)  (?<=\$) \w+ """,      // variable
    """(?x)  -? \b\d+(\.\d+)? """, // number
    """(?x)  " (\\"|[^"])* "  """, // string
    """(?x)  ' (\\'|[^'])* '  """, // string
    """(?xs) /\* .* \*/ """,       // comment
    """(?xm) //.*$ """             // comment
  )

  def colorFor(str: String) = str match {
    case str if str(0) == '$'   => Blue
    case str if str(0).isDigit  => Purple
    case str if str(0) == '-'   => Purple
    case str if str(0) == '/'   => Gray
    case _                      => Orange
  }

}

class JSHighlighter extends ProtoHighlighter {

  val tokens = Seq(
    Red   -> "for while do if return let var const document".split(" "),
    Green -> "null true false undefined new this =>".split(" ")
  )

  val regexes = Seq(
    """(?x)  -? \b\d+(\.\d+)? """, // number
    """(?x)  " (\\"|[^"])* "  """, // string
    """(?x)  ' (\\'|[^'])* '  """, // string
    """(?xs) /\* .* \*/ """,       // comment
    """(?xm) //.*$ """             // comment
  )

  def colorFor(str: String) = str match {
    case str if str(0).isDigit => Purple
    case str if str(0) == '-'  => Purple
    case str if str(0) == '"'  => Yellow
    case str if str(0) == '\'' => Yellow
    case str if str(0) == '/'  => Gray
    case _ => 0
  }

}

class CHighlighter extends ProtoHighlighter {
  val tokens = Seq(
    Red    -> "for return".split(" "),
    Blue   -> "int long short char void signed unsigned float double size_t struct".split(" ")
  )

  val regexes = Seq(
    """(?x)  -? \b\d+(\.\d+)? """, // number
    """(?x)  " (\\"|[^"])* "  """, // string
    """(?x)  ' (\\'|[^'])* '  """, // string
    """(?xs) /\* .* \*/ """,       // comment
    """(?xm) //.*$ """             // comment
  )

  def colorFor(str: String) = str match {
    case str if str(0).isDigit => Purple
    case str if str(0) == '-'  => Purple
    case str if str(0) == '"'  => Yellow
    case str if str(0) == '\'' => Yellow
    case str if str(0) == '/'  => Gray
    case _ => 0
  }
}
