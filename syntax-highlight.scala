package asciiblog

import java.lang.StringBuilder
import java.util.regex.{ Pattern, Matcher }
import scala.util.matching.Regex
import scala.collection.mutable
import Colors._
import ProtoHighlighter._
import FastHighlighter._


class Highlighters(highlighters: Map[String, Highlighter]) {
  def highlight(source: String, lang: String): String =
    highlighters.get(lang) match {
      case Some(hl) => applyHighlights(source, hl(source))
      case None     => source
    }

  def applyHighlights(source: String, lights: Seq[Light]): String = {
    val sb = new StringBuilder
    var lastEnd = 0
    for (l <- lights) {
      sb.append(html.escape(source.substring(lastEnd, l.start)))
      sb.append("<span class=hl"+l.color+">")
      sb.append(html.escape(source.substring(l.start, l.start+l.length)))
      sb.append("</span>")
      lastEnd = l.start+l.length
    }
    sb.append(html.escape(source.substring(lastEnd)))
    sb.toString
  }
}



object DefaultHighlighters extends Highlighters(Map(
  "scala"      -> new ScalaHighlighter,
  "java"       -> new JavaHighlighter,
  "php"        -> new PHPHighlighter,
  "javascript" -> new JSHighlighter,
  "c"          -> new CHighlighter
))


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
  def tokens(ts: String) = ts.split(" +").map(mkTokenRegex).mkString("|")

  private def mkTokenRegex(t: String): String =
    (if (t.head.isLetter) "\\b" else "") + quote(t) + (if (t.last.isLetter) "\\b" else "")
  private def quote(r: String) = if (r.matches("^[a-zA-Z0-9]+$")) r else Regex.quote(r)

  val comments = Seq(
    Gray   -> """(?xs) /\* .* \*/ """,
    Gray   -> """(?xm) //.*$ """
  )
}


sealed trait FastPattern
case class RegexPattern(guardRange: String, regex: String) extends FastPattern
case class Keywords(keywords: String) extends FastPattern

class FastHighlighter(patterns: (Int, FastPattern)*) extends Highlighter {
  import Character._

  private sealed trait Patt
  private case class LitPatt(str: String, color: Int) extends Patt
  private case class RgxPatt(p: Pattern, color: Int) extends Patt

  private[this] val realPatterns = Array.fill(256)(mutable.ArrayBuffer[Patt]())

  patterns.foreach {
    case (color, Keywords(kws)) =>
      kws.split(" ").foreach { kw =>
        realPatterns(kw.charAt(0)) += LitPatt(kw, color)
      }
    case (color, RegexPattern(guardRange, regex)) =>
      val p = RgxPatt(Pattern.compile(regex), color)
      """\G(?:.-.|.)""".r.findAllIn(guardRange)
        .flatMap { range => range.head to range.last }
        .foreach { r => realPatterns(r) += p }
  }

  def apply(source: String): Seq[Light] = {
    val res = Vector.newBuilder[Light]

    var i = 0; while (i < source.length) {
      val ch = source.charAt(i)

      if (ch >= realPatterns.length || !validStart(source, i)) {
        i += 1

      } else {
        val patterns = realPatterns(ch)
        var matched = false
        var pattIdx = 0; while (!matched && pattIdx < patterns.size) {
          patterns(pattIdx) match {
            case LitPatt(str, color) =>
              if (source.regionMatches(i, str, 0, str.length) && validEnd(source, i+str.length)) {
                res += Light(i, str.length, color)
                i += str.length
                matched = true
              }
            case RgxPatt(p, color) =>
              val m = p.matcher(source).region(i, source.length).useTransparentBounds(true)
              if (m.lookingAt) {
                res += Light(i, m.end - m.start, color)
                i = m.end
                matched = true
              }
          }
          pattIdx += 1
        }

        if (!matched) { i += 1 }
      }
    }

    res.result()
  }

  def validStart(source: String, i: Int): Boolean = {
    var ch = source.charAt(i)
    def prev = source.charAt(i-1)
    (isLetterOrDigit(ch) && (i == 0 || (prev != '_' && !isLetterOrDigit(prev)))) || (!isWhitespace(ch) && !isLetterOrDigit(ch))
  }

  def validEnd(source: String, i: Int): Boolean = {
    def ch = source.charAt(i)
    def prev = source.charAt(i-1)
    (i == source.length || (ch != '_' && !isLetterOrDigit(ch)) || (!isLetterOrDigit(prev) && (ch == '_' || isLetterOrDigit(ch))))
  }
}

object FastHighlighter {
  val blockComments       = Gray   -> RegexPattern("/",   """(?xs) /\* .* \*/ """ )
  val lineComments        = Gray   -> RegexPattern("/",   """(?xm) //.*$ """ )
  val doubleQuotedString  = Yellow -> RegexPattern("\"",  """(?x)  " (?:\\"|[^"])* "  """ )
  val singleQuotedString  = Yellow -> RegexPattern("'",   """(?x)  ' (?:\\'|[^'])* '  """ )
  val javaChar            = Yellow -> RegexPattern("'",   """(?x)  ' (?:\\'|.) '      """ )
  val javaNumber          = Purple -> RegexPattern("0-9", """(?x)  \d+ (?:\.\d+)? """ )
  val javaHexNumber       = Purple -> RegexPattern("0",   """(?x)  0x[a-fA-F0-9]+[lL]? """ )
}

// ==========

class ScalaHighlighter extends FastHighlighter(
  blockComments,
  lineComments,
  Red    -> Keywords("class trait object extends with var val def if else match case while do for type forSome try catch finally yield macro return"),
  Green  -> Keywords("null true false ne eq this super new override final private protected implicit import package sealed <- -> => require lazy"),
  Green  -> RegexPattern("@",   """(?x)  @\w+ """ ),                                // annotation
  Blue   -> RegexPattern("A-Z", """(?x)  [A-Z][\w$]* """ ),                         // type
  javaHexNumber,
  javaNumber,
  Yellow -> RegexPattern("\"",  """(?xs) \"\"\" .*? \"\"\" """ ),                   // string
  doubleQuotedString,
  javaChar,
  Green  -> RegexPattern("a-z", """(?x)  (?<=(?:val|var|def)\s{1,9}) [a-z][\w$]* """ ) // declaration
)

class JavaHighlighter extends FastHighlighter(
  blockComments,
  lineComments,
  Red    -> Keywords("class interface extends implements var if else case whitch while do for try catch finally return"),
  Green  -> Keywords("null true false this super new final private protected import package"),
  Blue   -> Keywords("int long double float boolean void byte char short"),
  Green  -> RegexPattern("@",   """(?x)  @\w+ """ ),                // annotation
  Blue   -> RegexPattern("A-Z", """(?x)  [A-Z]\w*\b """ ),          // type
  javaHexNumber,
  javaNumber,
  doubleQuotedString,
  javaChar
)

class PHPHighlighter extends FastHighlighter(
  blockComments,
  lineComments,
  Gray   -> Keywords("<?php ?>"),
  Red    -> Keywords("class extends for if else elsif declare while foreach as function return yield from echo die isset unset"),
  Red    -> Keywords("<=> -> => $ !== != === == ... << >> < > <= >= ++ -- ** + - * / % ~ . ! && || & ^ | ?? ?:"),
  Purple -> Keywords("true false null new clone instanceof"),
  Blue   -> Keywords("private protected var const abstract array (bool) (float) (double) (string) (int) (void)"),
  Green  -> Keywords("namespace use"),
  Orange -> RegexPattern("a-zA-Z", """(?x)  (?<=\$) \w+ """ ),        // variable
  Purple -> RegexPattern("0-9",    """(?x)  \d+ (?:\.\d+)? """ ),     // number
  doubleQuotedString,
  singleQuotedString
)

class JSHighlighter extends FastHighlighter(
  Red    -> Keywords("for while do if return let var const document function"),
  Green  -> Keywords("null true false undefined new this =>"),
  Purple -> RegexPattern("0-9", """(?x)  \d+ (?:\.\d+)? """), // number
  singleQuotedString,
  doubleQuotedString,
  blockComments,
  lineComments
)

class CHighlighter extends FastHighlighter(
  blockComments,
  lineComments,
  Red    -> Keywords("for return switch break continue"),
  Blue   -> Keywords("int long short char void signed unsigned float double size_t struct"),
  Orange -> Keywords("const"),
  Yellow -> Keywords("case"),
  Purple -> RegexPattern("0-9", """(?x)  \d+ (?:\.\d+)? """), // number
  Yellow -> RegexPattern("\"",  """(?x)  " (?:\\"|[^"])* "  """),  // string
  Yellow -> RegexPattern("'",   """(?x)  ' (?:\\'|[^'])* '  """),   // string
  doubleQuotedString,
  javaChar
)
