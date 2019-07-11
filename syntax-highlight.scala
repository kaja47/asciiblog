package asciiblog

import java.lang.StringBuilder
import java.util.regex.{ Pattern, Matcher }
import scala.util.matching.Regex
import scala.collection.mutable
import Colors._
import ProtoHighlighter._
import FastHighlighter._

object Highlighter {
  val highlighters: Map[String, Highlighter] = Map(
    "scala"      -> new ScalaHighlighter2,
    "java"       -> new JavaHighlighter2,
    "php"        -> new PHPHighlighter2,
    "javascript" -> new JSHighlighter2,
    "c"          -> new CHighlighter2
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
  val blockComments = Gray -> RegexPattern("/",   """(?xs) /\* .* \*/ """ )
  val lineComments  = Gray -> RegexPattern("/",   """(?xm) //.*$ """ )
}


class ScalaHighlighter2 extends FastHighlighter(
  Red    -> Keywords("class trait object extends with var val def if else match case while do for type forSome try catch finally yield macro return"),
  Green  -> Keywords("null true false ne eq this super new override final private protected implicit import package sealed <- -> => require lazy"),
  Green  -> RegexPattern("@",   """(?x)  @\w+ """ ),                                // annotation
  Blue   -> RegexPattern("A-Z", """(?x)  [A-Z][\w$]* """ ),                         // type
  Purple -> RegexPattern("0",   """(?x)  0x[a-fA-F0-9]+[lL]? """ ),                 // hex number
  Purple -> RegexPattern("0-9", """(?x)  \d+ (?:\.\d+)? """ ),                   // number
  Yellow -> RegexPattern("\"",  """(?xs) \"\"\" .*? \"\"\" """ ),                   // string
  Yellow -> RegexPattern("\"",  """(?x)  " (?:\\"|[^"])* "   """ ),                 // string
  Yellow -> RegexPattern("'",   """(?x)  ' (?:\\'|.) ' """ ),                       // char
  Green  -> RegexPattern("a-z", """(?x)  (?<=(?:val|var|def)\s{1,9}) [a-z][\w$]* """ ), // declaration
  blockComments,
  lineComments
)


class ScalaHighlighter extends ProtoHighlighter(Seq(
  Red    -> tokens("class trait object extends with var val def if else match case while do for type forSome try catch finally yield macro return"),
  Green  -> tokens("null true false ne eq this super new override final private protected implicit import package sealed <- -> => require lazy"),
  Green  -> """(?x)  @\w+ """,                 // annotation
  Blue   -> """(?x)  \b[A-Z][\w$]*\b """,      // type
  Purple -> """(?x)  \b0x[a-fA-F0-9]+[lL]? """,// hex number
  Purple -> """(?x)  -? \b\d+ (?:\.\d+)? """,  // number
  Yellow -> """(?xs) \"\"\" .*? \"\"\" """,    // string
  Yellow -> """(?x)  " (?:\\"|[^"])* "   """,  // string
  Yellow -> """(?x)  ' (?:\\'|.) ' """,        // char
  Green  -> """(?x)  (?<=(?:val|var|def)\s{1,9}) [a-z][\w$]* """ // declaration
) ++ comments)


class JavaHighlighter2 extends FastHighlighter(
  Red    -> Keywords("class interface extends implements var if else case whitch while do for try catch finally return"),
  Green  -> Keywords("null true false this super new final private protected import package"),
  Blue   -> Keywords("int long double float boolean void byte char short"),
  Green  -> RegexPattern("@",   """(?x)  @\w+ """ ),                 // annotation
  Blue   -> RegexPattern("A-Z", """(?x)  [A-Z]\w*\b """ ),         // type
  Purple -> RegexPattern("0",   """(?x)  0x[a-fA-F0-9]+[lL]? """ ),// hex number
  Purple -> RegexPattern("0-9", """(?x)  \d+ (?:\.\d+)? """ ),     // number
  Yellow -> RegexPattern("\"",  """(?x)  " (?:\\"|[^"])* "   """ ),  // string
  Yellow -> RegexPattern("'",   """(?x)  ' (?:\\'|.) ' """ ),        // char
  blockComments,
  lineComments
)

class JavaHighlighter extends ProtoHighlighter(Seq(
  Red    -> tokens("class interface extends implements var if else case whitch while do for try catch finally return"),
  Green  -> tokens("null true false this super new final private protected import package"),
  Blue   -> tokens("int long double float boolean void byte char short"),
  Green  -> """(?x)  @\w+ """,                 // annotation
  Blue   -> """(?x)  \b[A-Z]\w*\b """,         // type
  Purple -> """(?x)  \b0x[a-fA-F0-9]+[lL]? """,// hex number
  Purple -> """(?x)  \b\d+ (?:\.\d+)? """,  // number
  Yellow -> """(?x)  " (?:\\"|[^"])* "   """,  // string
  Yellow -> """(?x)  ' (?:\\'|.) ' """,        // char
) ++ comments)


class PHPHighlighter2 extends FastHighlighter(
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
  Yellow -> RegexPattern("\"",     """(?x)  " (?:\\"|[^"])* "  """ ), // string
  Yellow -> RegexPattern("''",     """(?x)  ' (?:\\'|[^'])* '  """ ), // string
)


class PHPHighlighter extends ProtoHighlighter(comments ++ Seq(
  Gray   -> tokens("<?php ?>"),
  Red    -> tokens("class extends for if else elsif declare while foreach as function return yield from echo die isset unset <=> -> => $ !== != === == ... << >> < > <= >= ++ -- ** + - * / % ~ . ! && || & ^ | ?? ?:"),
  Purple -> tokens("true false null new clone instanceof"),
  Blue   -> tokens("private protected var const abstract array (bool) (float) (double) (string) (int) (void)"),
  Green  -> tokens("namespace use"),
  Orange -> """(?x)  (?<=\$) \w+ """,         // variable
  Purple -> """(?x)  -? \b\d+ (?:\.\d+)? """, // number
  Yellow -> """(?x)  " (?:\\"|[^"])* "  """,  // string
  Yellow -> """(?x)  ' (?:\\'|[^'])* '  """   // string
)) {
}


class JSHighlighter2 extends FastHighlighter(
  Red    -> Keywords("for while do if return let var const document function"),
  Green  -> Keywords("null true false undefined new this =>"),
  Purple -> RegexPattern("0-9", """(?x)  \d+ (?:\.\d+)? """), // number
  Yellow -> RegexPattern("\"",  """(?x)  " (?:\\"|[^"])* "  """),  // string
  Yellow -> RegexPattern("'",   """(?x)  ' (?:\\'|[^'])* '  """),   // string
  blockComments,
  lineComments
)

class JSHighlighter extends ProtoHighlighter(Seq(
  Red    -> tokens("for while do if return let var const document function"),
  Green  -> tokens("null true false undefined new this =>"),
  Purple -> """(?x)  -? \b\d+ (?:\.\d+)? """, // number
  Yellow -> """(?x)  " (?:\\"|[^"])* "  """,  // string
  Yellow -> """(?x)  ' (?:\\'|[^'])* '  """   // string
) ++ comments)


class CHighlighter2 extends FastHighlighter(
  Red    -> Keywords("for return switch break continue"),
  Blue   -> Keywords("int long short char void signed unsigned float double size_t struct"),
  Orange -> Keywords("const"),
  Yellow -> Keywords("case"),
  Purple -> RegexPattern("0-9", """(?x)  \d+ (?:\.\d+)? """), // number
  Yellow -> RegexPattern("\"",  """(?x)  " (?:\\"|[^"])* "  """),  // string
  Yellow -> RegexPattern("'",   """(?x)  ' (?:\\'|[^'])* '  """),   // string
  blockComments,
  lineComments
)

class CHighlighter extends ProtoHighlighter(Seq(
  Red    -> tokens("for return switch break continue"),
  Blue   -> tokens("int long short char void signed unsigned float double size_t struct"),
  Orange -> tokens("const"),
  Yellow -> tokens("case"),
  Purple -> """(?x)  -? \b\d+ (?:\.\d+)? """, // number
  Yellow -> """(?x)  " (?:\\"|[^"])* "  """,  // string
  Yellow -> """(?x)  ' (?:\\'|[^'])* '  """   // string
) ++ comments)
