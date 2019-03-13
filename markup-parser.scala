package asciiblog

import java.lang.StringBuilder
import Character._

case class MarkupBlock(
  val startToken: String, val endToken: String,
  val startTag: String,   val endTag: String,
  val recur: Boolean,
  val tight: Boolean,
  val doubleQuotes: Boolean = false,
  val compact: Boolean = false
) {
  val startChar = startToken.charAt(0)
  val startLong = startToken.length > 1

  val endChar   = endToken.charAt(0)
  val endLong   = endToken.length > 1

  def startLen = startToken.length
  def endLen   = endToken.length

  val attributePos = startTag.indexOf('>')

  lazy val asCompact = if (tight) copy(compact = true) else sys.error("non tight blocks cannot have a compact version")

  // long mode for multiple words
  // non-alpha-num START non-whitspace ... non-whitespace END non-alpha-num
  //
  // compact mode for single word
  // START non-whitespace* END


  def checkStart(str: String, i: Int) =
    str.regionMatches(i, startToken, 0, startToken.length)

  def isLongStart(str: String, i: Int) =
    if (!tight) true else (i == 0 || !isLetterOrDigit(str(i-1))) && (i+startLen >= str.length || !isWhitespace(str(i+startLen)))

  def isCompactStart(str: String, i: Int) =
    i+startLen == str.length || !isWhitespace(str(i+startLen))

  def isStart(str: String, i: Int) =
    isLongStart(str, i) || isCompactStart(str, i)


  def checkEnd(str: String, i: Int) =
    str.regionMatches(i, endToken, 0, endToken.length)

  def isLongEnd(str: String, i: Int) =
    if (!tight) true else (i == 0 || !isWhitespace(str(i-1))) && (i+endLen >= str.length || !isLetterOrDigit(str(i+endLen)))

  def isCompactEnd(str: String, i: Int) =
    i == 0 || !isWhitespace(str(i-1))

  def isEnd(str: String, i: Int) =
    isLongEnd(str, i) || isCompactEnd(str, i)


  def appendBody(str: String, from: Int, to: Int, sb: StringBuilder, p: MarkupParser) = {
    require(!recur)
    sb.append(str, from, to)
  }

  override def toString = s"MarkupBlock($startToken...$endToken, ${if (compact) "compact" else "long"})"
}





object MarkupParser {
  val blocks = Array(
    new MarkupBlock("<!--", "-->", "",    "",        recur = false, tight = false) {
      override def appendBody(str: String, from: Int, to: Int, sb: StringBuilder, p: MarkupParser) = sb
    },
    new MarkupBlock("[|", "|]", "",       "",        recur = false, tight = false) {
      override def appendBody(str: String, from: Int, to: Int, sb: StringBuilder, p: MarkupParser) = {
        val len = to - from - 2
        for (i <- 0 until (len/5)) { sb.append("█████").append("<wbr>") }
        sb.append("█████")
      }
    },
    new MarkupBlock("`",  "`",  "<code>", "</code>", recur = false, tight = false) {
      override def appendBody(str: String, from: Int, to: Int, sb: StringBuilder, p: MarkupParser) =
        util.escape(str, from, to, sb)
    },
    new MarkupBlock("''", "''", "",       "",        recur = false, tight = false),
    new MarkupBlock("**", "**", "<b>",    "</b>",    recur = true, tight = true),
    new MarkupBlock("*",  "*",  "<i>",    "</i>",    recur = true, tight = true),
    new MarkupBlock("//", "//", "<i>",    "</i>",    recur = true, tight = true) {
      override def checkStart(str: String, i: Int) = super.checkStart(str, i) && (i == 0 || str.charAt(i-1) != ':')
      override def checkEnd  (str: String, i: Int) = super.checkEnd  (str, i) && (i == 0 || str.charAt(i-1) != ':')
    },
    new MarkupBlock("\"", "\"", "",       "",        recur = true, tight = true, doubleQuotes = true),
    new MarkupBlock("[[", "]]", "",       "",        recur = false, tight = false) {
      override def appendBody(str: String, from: Int, to: Int, sb: StringBuilder, p: MarkupParser) = {
        val link = /* TODO p.processLink*/("#fn"+str.substring(from, to))
        sb.append("<a href=").append(link).append("><sup>").append(str, from, to).append("</sup></a> ")
      }
      override def checkStart(str: String, i: Int) = super.checkStart(str, i) && (i+4 < str.length && Character.isDigit(str.charAt(i+2)))
    },
  )

  val specialChars: Array[Char] = blocks.flatMap { b => Seq(b.startToken(0), b.endToken(0)) }.toArray
  val largestSpecialChar: Char = specialChars.max
  val specialCharMask = Array.tabulate[Boolean](largestSpecialChar+1) { i => specialChars.contains(i.toChar) }

  val allQuoteMarks = Map(
    "cz" -> Seq("„|“", "‚|‘", "»|«", "›|‹"),
    "en" -> Seq("“|”", "‘|’"),
    "fr" -> Seq("«&nbsp;|&nbsp;»"),
  ).map { case (l, qs) => (l, qs.map{ qs => val Array(a, b) = qs.split("\\|"); (a, b) })}

  val preposCharsLen = 123
  val preposChars = Array.tabulate[Boolean](preposCharsLen) { i => "ksvzouiaKSVZOUIA".indexOf(i) != -1 }
  def isPreposChar(ch: Char) = ch < preposCharsLen && preposChars(ch)

  def handleCZPrepositions(txt: String) =
    if (txt.length < 2) txt else {
      def ws(ch: Char) = (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r')

      val res = new StringBuilder(txt.length+128)
      var pos = 0

      var i = 2; while (i < txt.length) {
        if (ws(txt.charAt(i)) && isPreposChar(txt.charAt(i-1)) && ws(txt.charAt(i-2))) {
          res.append(txt, pos, i).append("\u00A0")
          pos = i+1
        }
        i += 1
      }
      res.append(txt, pos, txt.length)
      res.toString
    }

  val replacements = Seq(
    "---" -> "&mdash;",
    "--"  -> "&ndash;",
    "..." -> "…",
    "->"  -> "→",
    "<-"  -> "←",
    "<->" -> "↔",
    "(TM)"-> "™",
    "(R)" -> "®",
    " - " -> " &ndash; ",
  )

  def findMods(line: String): Option[(Mods, Int)] =
    findMods(line, 0, line.length)

  // .(title)  .[class1 class2 #id]  .{color:blue}  .<  .>  .<>  .=
  def findMods(line: String, from: Int, to: Int): Option[(Mods, Int)] = {
    val s = new Slurp(line, from, to)
    while (s.to('.').matches && !s.touchesEnd) {
      s.ignore()
      var mods = Mods()
      val start = s.pos
      var ok = true
      while (ok && !s.touchesEnd) {
        ok = modAlternatives.exists { case (f, ex) =>
          f(s)
          if (s.matches) {
            val x = ex(s)
            s.ignore()
            mods = mods.merge(x)
            true
          } else {
            s.unmatch().tryAgain()
            false
          }
        }
      }
      if (ok) return Some((mods, start-1))
    }
    None
  }

  private val modAlternatives = Seq[(Slurp => Unit, Slurp => Mods)](
    (_.quoted('(', ')'), s => Mods(title   = s.asString(1, -1))),
    (_.quoted('[', ']'), s => Mods(classes = s.asString(1, -1))),
    (_.quoted('{', '}'), s => Mods(styles  = s.asString(1, -1))),
    (_.string("<>"),     s => Mods(styles = "text-align:center")),
    (_.string(">"),      s => Mods(styles = "text-align:right")),
    (_.string("<"),      s => Mods(styles = "text-align:left")),
    (_.string("="),      s => Mods(styles = "text-align:justify"))
  )

  def mkMods(m: Mods): String = {
    if (m.isEmpty) return "" // the most mods are always empty

    val (ids, classes) =
      if (m.classes.isEmpty) (Array[String](), Array[String]())
      else m.classes.split(" ").partition(_.startsWith("#"))

    (if (m.title.nonEmpty)  " title="+util.quoteHTMLAttribute(m.title)                       else "")+
    (if (classes.nonEmpty)  " class="+util.quoteHTMLAttribute(classes.mkString(" "))         else "")+
    (if (ids.nonEmpty)      " id="   +util.quoteHTMLAttribute(ids.map(_.tail).mkString(" ")) else "")+
    (if (m.styles.nonEmpty) " style="+util.quoteHTMLAttribute(m.styles)                      else "")
  }

}

case class Mods(title: String = "", classes: String = "", styles: String = "") {
  def join(a: String, b: String, delim: String) = if (a.isEmpty || b.isEmpty) a+b else a+delim+b
  def merge(m: Mods) = Mods(join(title, m.title, " "), join(classes, m.classes, " "), join(styles, m.styles, ";"))
  def isEmpty = title.isEmpty && classes.isEmpty && styles.isEmpty
}


class MarkupParser(val lang: String, val processLink: String => String) {
  import MarkupParser._

  val quoteMarks = allQuoteMarks(lang).head

  def apply(s: String) = {
    val sb = new StringBuilder
    processMarkup(s, 0, sb, null)
    sb.toString
  }

  // TODO hack
  private[this] var seenWhitespace = false

  // vrátí pozici speciálního znaku nebo str.length, když došlo na konec
  def nextSpecialCharacter(str: String, from: Int, includingWhitespace: Boolean): Int = {
    var i = from
    while (i < str.length) {
      val ch = str.charAt(i)
      //if (specialChars.indexOf(ch) != -1) return i
      if (ch <= largestSpecialChar && specialCharMask(ch)) return i
      val ws = ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t' //  isWhitespace(ch)
      if (ws) seenWhitespace = true
      if (ws && includingWhitespace) return i
      i += 1
    }
    i
  }

  def identifyBlock(str: String, i: Int, outer: MarkupBlock): MarkupBlock = {
    var j = 0; while (j < blocks.length) {
      val b = blocks(j)

      if (b.checkStart(str, i) && b != outer) { // its not posible to nest identical blocks
        if (b.isLongStart(str, i)) {
          seenWhitespace = false
          return b
        } else if (b.tight && b.isCompactStart(str, i)) {
          return b.asCompact
        }
      }

      j += 1
    }
    null
  }

  def appendText(str: String, from: Int, to: Int, sb: StringBuilder) = {
    var seg = str.substring(from, to)
    if (lang == "cz")                       { seg = handleCZPrepositions(seg) }
    for ((search, replace) <- replacements) { seg = seg.replace(search, replace) }
    sb.append(seg)
  }

  def blockEnds(str: String, i: Int, outer: MarkupBlock): Boolean = {
    if (outer.checkEnd(str, i)) {
      if (outer.compact && outer.isEnd(str, i)) return true
      if (!outer.compact && !seenWhitespace && outer.isCompactEnd(str, i)) return true
      if (!outer.compact && outer.isLongEnd(str, i)) return true
    }
    false

    //if (outer.compact) {
    //  if (isWhitespace(str(i))) return -1
    //  if (outer.checkEnd(str, i) && outer.isEnd(str, i)) return i+outer.endLen
    //} else {
    //  if (outer.checkEnd(str, i)) {
    //    if (!seenWhitespace && outer.isCompactEnd(str, i)) return i+outer.endLen
    //    if (outer.isLongEnd(str, i)) return i+outer.endLen
    //  }
    //}
  }

  case class ParseResult(end: Int, mods: Mods = null)

  // vrací pozici, kde úspěšně skončilo parsování
  // nebo -1, pokud nebyl nalezen koncový tag
  def processMarkup(str: String, pos: Int, sb: StringBuilder, outer: MarkupBlock): ParseResult = {
    var i = pos

    do {
      var mark = i
      val searchForWhitespace = outer != null && outer.compact
      var block: MarkupBlock = null

      do {
        i = nextSpecialCharacter(str, i, searchForWhitespace)

        if (i >= str.length) {
          appendText(str, mark, i, sb)
          return ParseResult(-1)
        }
        // jsem uvnitř bloku, je tohle jeho uzavírka?
        // kompaktní bloky nemůžou obsahovat mezery
        if (outer != null && outer.compact && isWhitespace(str.charAt(i))) {
          appendText(str, mark, i, sb);
          return ParseResult(-1)
        }
        if (outer != null && blockEnds(str, i, outer)) {
          findMods(str, mark, i) match {
            case Some((mods, modsStart)) =>
              appendText(str, mark, modsStart-1, sb)
              return ParseResult(i+outer.endLen, mods)
            case None =>
              appendText(str, mark, i, sb)
              return ParseResult(i+outer.endLen)
          }

        }

        block = identifyBlock(str, i, outer)

        if (block == null) {
          i += 1
        }

      } while (block == null)

      appendText(str, mark, i, sb)


      val outerTagStart = sb.length
      sb.append(block.startTag)

      i += block.startToken.length
      val innerStart = i

      if (!block.recur) {
        require(!block.tight)
        var endPos = i-1
        do {
          endPos = str.indexOf(block.endToken, endPos+1)
        } while (!(endPos == -1 || block.checkEnd(str, endPos))) // jestli to není tight, pak stačí kontrolovat jen checkEnd

        if (endPos == -1) {
          sb.setLength(outerTagStart)
          sb.append(block.startToken)

        } else {
          i = endPos+block.endToken.length

          block.appendBody(str, innerStart, endPos, sb, this)
          sb.append(block.endTag)
          seenWhitespace = false

          val posOfEndSqBraqcket = findLink(str, i)
          if (posOfEndSqBraqcket != -1) {
            val link = processLink(str.substring(i+2, posOfEndSqBraqcket))
            if (link != null) {
              sb.insert(outerTagStart, "<a href="+util.quoteHTMLAttribute(link)+">").append("</a>")
            }
            i = posOfEndSqBraqcket+1
          }
        }

      } else {

        val ParseResult(endPos, mods) = processMarkup(str, i, sb, block)
        if (endPos <= -1) {
          // element nemá konec, vrátit zpátky počáteční tag
          sb.setLength(outerTagStart)
          sb.append(block.startToken)

        } else {

          i = endPos
          sb.append(block.endTag)
          //seenWhitespace = false TODO
          
          if (mods != null && !block.doubleQuotes) {
            sb.insert(outerTagStart+block.attributePos, mkMods(mods))
          }

          val posOfEndSqBraqcket = findLink(str, i)
          if (posOfEndSqBraqcket != -1) {
            val link = processLink(str.substring(i+2, posOfEndSqBraqcket))
            if (link != null) {
              if (block.doubleQuotes && mods != null) {
                sb.insert(outerTagStart, "<a"+mkMods(mods)+" href="+util.quoteHTMLAttribute(link)+">").append("</a>")
              } else {
                sb.insert(outerTagStart, "<a href="+util.quoteHTMLAttribute(link)+">").append("</a>")
              }
            }
            i = posOfEndSqBraqcket+1
          }

          if (block.doubleQuotes && posOfEndSqBraqcket == -1) {
            if (mods == null) {
              val (a, b) = quoteMarks
              sb.insert(outerTagStart, a)
              sb.append(b)
            } else {
              sb.insert(outerTagStart, "<span"+mkMods(mods)+">")
              sb.append("</span>")
            }
          }

        }
      }

    } while (i < str.length)

    if (i < str.length) {
      //println(str.substring(i, str.length))
      sb.append(str, i, str.length)
    }

    if (i >= str.length) ParseResult(-1) else ParseResult(i)
  }


  // suffix  :[...]
  def findLink(str: String, pos: Int): Int = {
    if (pos+1 >= str.length || str.charAt(pos) != ':' || str.charAt(pos+1) != '[') return -1
    var i = pos+2 ; while (i < str.length && str.charAt(i) != ']') {
      i += 1
    }
    if (i == str.length) -1 else i
  }
}
