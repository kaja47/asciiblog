package asciiblog

import java.lang.StringBuilder
import scala.collection.mutable
import scala.util.matching.Regex

object Highlighter {
  val highlighters: Map[String, Highlighter] = Map(
    "scala"      -> new ScalaHighlighter,
    "php"        -> new PHPHighlighter,
    "javascript" -> new JSHighlighter
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

  def tokens: Map[Int, Seq[String]]

  def mkTokenRegex(t: String): String = t

  def regexes: Seq[String]

  def colorFor(str: String): Int

  lazy val tokenColors: Map[String, Int] =
    tokens.flatMap { case (c, ts) => ts.map(t => (t, c)) }.toMap

  def _colorFor(str: String): Int =
    tokenColors.get(str).getOrElse(colorFor(str))

  lazy val regex = ((regexes :+ tokens.values.flatten.map(mkTokenRegex).mkString("|")): Seq[String])
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

  val tokens = Map(
    0 -> "class trait extends with var val def if match case while do for type".split(" "),
    1 -> "null true false this super new override final private protected import package <- -> =>".split(" ")
  )

  override def mkTokenRegex(t: String): String =
    (if (t.head.isLetter) "\\b" else "") + scala.util.matching.Regex.quote(t) + (if (t.last.isLetter) "\\b" else "")

  val regexes = Seq(
    """(?x)  @\w+ """,              // annotation
    """(?x)  \b[A-Z]\w*\b """,      // type
    """(?x)  -? \b\d+ """,          // number
    """(?xs) \"\"\" .*? \"\"\" """, // string
    """(?x)  " (\\"|[^"])* "   """, // string
    """(?x)  ' (\\'|.) ' """,       // char
    """(?xs) /\* .* \*/ """,        // comment
    """(?xm) //.*$ """              // comment
  )

  def colorFor(str: String) = str match {
    case str if str(0) == '@'  => 1
    case str if str(0).isUpper => 2
    case str if str(0).isDigit => 3
    case str if str(0) == '-'  => 3
    case str if str(0) == '"'  => 4
    case str if str(0) == '\'' => 4
    case str if str(0) == '/'  => 5
  }
}

class PHPHighlighter extends ProtoHighlighter {

  val tokens = Map(
    Gray -> "<?php".split(" "),
    Red  -> "class for if declare while foreach as function return -> => $ == === ... != !== < > <= >= - + * / .".split(" "),
    Purple -> "true false null".split(" "),
    Blue -> "private protected var const".split(" ")
  )

  override def mkTokenRegex(t: String): String =
    (if (t.head.isLetter) "\\b" else "") + scala.util.matching.Regex.quote(t) + (if (t.last.isLetter) "\\b" else "")

  val regexes = Seq(
    """(?x) (?<=\$) \w+ """,       // variable
    """(?x)  -? \b\d+ """,         // number
    """(?x)  " (\\"|[^"])* "  """, // string
    """(?x)  ' (\\'|[^'])* '  """, // string
    """(?xs) /\* .* \*/ """,       // comment
    """(?xm) //.*$ """             // comment
  )

  def colorFor(str: String) = str match {
    case str if str(0) == '$'  => Blue
    case str if str(0).isDigit => Purple
    case str if str(0) == '-'  => Purple
    case str if str(0) == '/'  => Gray
    case _                     => Orange
  }

}

class JSHighlighter extends ProtoHighlighter {

  val tokens = Map(
    0 -> "for while do if return let var const document".split(" "),
    1 -> "null true false undefined new this =>".split(" ")
  )

  override def mkTokenRegex(t: String): String =
    (if (t.head.isLetter) "\\b" else "") + scala.util.matching.Regex.quote(t) + (if (t.last.isLetter) "\\b" else "")

  val regexes = Seq(
    """(?x)  -? \b\d+ """,         // number
    """(?x)  " (\\"|[^"])* "  """, // string
    """(?x)  ' (\\'|[^'])* '  """, // string
    """(?xs) /\* .* \*/ """,       // comment
    """(?xm) //.*$ """             // comment
  )

  def colorFor(str: String) = str match {
    case str if str(0).isDigit => 3
    case str if str(0) == '-'  => 3
    case str if str(0) == '"'  => 4
    case str if str(0) == '\'' => 4
    case str if str(0) == '/'  => 5
    case _ => 0
  }

}
