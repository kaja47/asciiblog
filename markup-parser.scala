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


  def appendBody(str: String, from: Int, to: Int, sb: StringBuilder, plaintext: Boolean) = {
    require(!recur)
    sb.append(str, from, to)
  }

  override def toString = s"MarkupBlock($startToken...$endToken, ${if (compact) "compact" else "long"})"
}





object MarkupParser {
  val blocks = Array(
    new MarkupBlock("<", ">", "<",   ">",             recur = false, tight = false) {
      override def appendBody(str: String, from: Int, to: Int, sb: StringBuilder, plaintext: Boolean) = sb.append(str, from, to)
      override def checkStart(str: String, i: Int) = super.checkStart(str, i) && validTags.contains(detectTagName(str, i+1))

      def detectTagName(str: String, from: Int) = {
        var f = from
        var i = from
        if (i < str.length && str.charAt(i) == '/') { f += 1; i += 1 }
        while (i < str.length && isLetterOrDigit(str.charAt(i))) i += 1
        str.substring(f, i)
      }
    },
    new MarkupBlock("<!--", "-->", "",    "",        recur = false, tight = false) {
      override def appendBody(str: String, from: Int, to: Int, sb: StringBuilder, plaintext: Boolean) = sb
    },
    new MarkupBlock("[|", "|]", "",       "",        recur = false, tight = false) {
      override def appendBody(str: String, from: Int, to: Int, sb: StringBuilder, plaintext: Boolean) = {
        if (!plaintext) {
          val len = to - from - 2
          for (i <- 0 until (len/5)) { sb.append("█████").append("<wbr>") }
        }
        sb.append("█████")
      }
    },
    new MarkupBlock("`",  "`",  "<code>", "</code>", recur = false, tight = false) {
      override def appendBody(str: String, from: Int, to: Int, sb: StringBuilder, plaintext: Boolean) =
        html.escape(str, from, to, sb)
    },
    new MarkupBlock("''", "''", "",       "",        recur = false, tight = false),
    new MarkupBlock("**", "**", "<b>",    "</b>",    recur = true, tight = true),
    new MarkupBlock("*",  "*",  "<i>",    "</i>",    recur = true, tight = true),
    new MarkupBlock("//", "//", "<i>",    "</i>",    recur = true, tight = true) {
      override def checkStart(str: String, i: Int) = super.checkStart(str, i) && (i == 0 || str.charAt(i-1) != ':') // check for https://
      override def checkEnd  (str: String, i: Int) = super.checkEnd  (str, i) && (i == 0 || str.charAt(i-1) != ':')
    },
    new MarkupBlock("\"", "\"", "",       "",        recur = true, tight = true, doubleQuotes = true),
    new MarkupBlock("[[", "]]", "",       "",        recur = false, tight = false) {
      override def appendBody(str: String, from: Int, to: Int, sb: StringBuilder, plaintext: Boolean) = {
        if (!plaintext) {
          sb.append("<a href=").append("#fn"+str.substring(from, to)).append("><sup>").append(str, from, to).append("</sup></a> ")
        }
        sb
      }
      override def checkStart(str: String, i: Int) = super.checkStart(str, i) && (i+4 < str.length && Character.isDigit(str.charAt(i+2)))
    },
  )

  val specialChars: Array[Char] = blocks.flatMap { b => Seq(b.startToken(0), b.endToken(0)) }.toArray
  val largestSpecialChar: Char = specialChars.max
  val specialCharMask = Array.tabulate[Boolean](largestSpecialChar+1) { i => specialChars.contains(i.toChar) }


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

    (if (m.title.nonEmpty)  " title="+html.quoteAttribute(m.title)                       else "")+
    (if (classes.nonEmpty)  " class="+html.quoteAttribute(classes.mkString(" "))         else "")+
    (if (ids.nonEmpty)      " id="   +html.quoteAttribute(ids.map(_.tail).mkString(" ")) else "")+
    (if (m.styles.nonEmpty) " style="+html.quoteAttribute(m.styles)                      else "")
  }

  val validTags = html5Tags

  def html5Tags = """a abbr acronym address applet area article aside audio b base basefont bdi bdo bgsound big blink blockquote body br button canvas caption center cite code col colgroup command content data datalist dd del details dfn dialog dir div dl dt element em embed fieldset figcaption figure font footer form frame frameset h1 h2 h3 h4 h5 h6 head header hgroup hr html i iframe image img input ins isindex kbd keygen label legend li link listing main map mark marquee menu menuitem meta meter multicol nav nextid nobr noembed noframes noscript object ol optgroup option output p param picture plaintext pre progress q rb rp rt rtc ruby s samp script section select shadow slot small source spacer span strike strong style sub summary sup table tbody td template textarea tfoot th thead time title tr track tt u ul var video wbr xmp""".split("\\s+").toSet

  def mathMLTags = """annotation annotation-xml maction maligngroup malignmark math menclose merror mfenced mfrac mglyph mi mlabeledtr mlongdiv mmultiscripts mn mo mover mpadded mphantom mprescripts mroot mrow ms mscarries mscarry msgroup msline mspace msqrt msrow mstack mstyle msub msubsup msup mtable mtd mtext mtr munder munderover none semantics""".split("\\s+").toSet

  def svgTags = """a animate animateMotion animateTransform circle clipPath color-profile defs desc discard ellipse feBlend feColorMatrix feComponentTransfer feComposite feConvolveMatrix feDiffuseLighting feDisplacementMap feDistantLight feDropShadow feFlood feFuncA feFuncB feFuncG feFuncR feGaussianBlur feImage feMerge feMergeNode feMorphology feOffset fePointLight feSpecularLighting feSpotLight feTile feTurbulence filter foreignObject g hatch hatchpath image line linearGradient marker mask mesh meshgradient meshpatch meshrow metadata mpath path pattern polygon polyline radialGradient rect script set solidcolor stop style svg switch symbol text textPath title tspan unknown use view""".split("\\s+").toSet

}

case class Mods(title: String = "", classes: String = "", styles: String = "") {
  def join(a: String, b: String, delim: String) = if (a.isEmpty || b.isEmpty) a+b else a+delim+b
  def merge(m: Mods) = Mods(join(title, m.title, " "), join(classes, m.classes, " "), join(styles, m.styles, ";"))
  def isEmpty = title.isEmpty && classes.isEmpty && styles.isEmpty
}


class MarkupParser(typography: Typography = NoTypography) {
  import MarkupParser._

  val quoteMarks = typography.doubleQuoteMarks

  def apply(s: String, processLink: String => String, plaintext: Boolean = false) = {
    val sb = new StringBuilder
    processMarkup(s, 0, sb, null, processLink, plaintext)
    sb.toString
  }

  def blockEnds(str: String, i: Int, outer: MarkupBlock, seenWS: Boolean): Boolean = {
    if (outer.checkEnd(str, i)) {
      if (outer.compact && outer.isEnd(str, i)) return true
      if (!outer.compact && !seenWS && outer.isCompactEnd(str, i)) return true
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
  def processMarkup(str: String, pos: Int, sb: StringBuilder, outer: MarkupBlock, processLink: String => String, plaintext: Boolean): ParseResult = {
    var i = pos

    def appendText(str: String, from: Int, to: Int, sb: StringBuilder) = {
      val seg = typography.apply(str.substring(from, to), plaintext)
      sb.append(seg.replace("<", "&lt;").replace(">", "&gt;"))
    }

    var seenWhitespace = false

    // vrátí pozici speciálního znaku nebo str.length, když došlo na konec
    def nextSpecialCharacter(str: String, from: Int, includingWhitespace: Boolean): Int = {
      var i = from
      while (i < str.length) {
        val ch = str.charAt(i)
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
            seenWhitespace = false // TODO what does this do anyway?
            return b
          } else if (b.tight && b.isCompactStart(str, i)) {
            return b.asCompact
          }
        }

        j += 1
      }
      null
    }

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
        if (outer != null && blockEnds(str, i, outer, seenWhitespace)) {
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
      if (!plaintext) {
        sb.append(block.startTag)
      }

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

          block.appendBody(str, innerStart, endPos, sb, plaintext)
          if (!plaintext) {
            sb.append(block.endTag)
          }
          seenWhitespace = false

          val posOfEndSqBracket = findLink(str, i)
          if (posOfEndSqBracket != -1 && !plaintext) {
            val link = processLink(str.substring(i+2, posOfEndSqBracket))
            if (link != null && !plaintext) {
              sb.insert(outerTagStart, "<a href="+html.quoteAttribute(link)+">").append("</a>")
            }
            i = posOfEndSqBracket+1
          }
        }

      } else {

        val ParseResult(endPos, mods) = processMarkup(str, i, sb, block, processLink, plaintext)
        if (endPos <= -1) {
          // element nemá konec, vrátit zpátky počáteční tag
          sb.setLength(outerTagStart)
          sb.append(block.startToken)

        } else {

          i = endPos
          if (!plaintext) {
            sb.append(block.endTag)
          }
          //seenWhitespace = false TODO

          if (mods != null && !block.doubleQuotes && !plaintext) {
            sb.insert(outerTagStart+block.attributePos, mkMods(mods))
          }

          val posOfEndSqBracket = findLink(str, i)
          if (posOfEndSqBracket != -1) {
            val link = processLink(str.substring(i+2, posOfEndSqBracket))
            if (link != null && !plaintext) {
              if (block.doubleQuotes && mods != null) {
                sb.insert(outerTagStart, "<a"+mkMods(mods)+" href="+html.quoteAttribute(link)+">").append("</a>")
              } else {
                sb.insert(outerTagStart, "<a href="+html.quoteAttribute(link)+">").append("</a>")
              }
            }
            i = posOfEndSqBracket+1
          }

          if (block.doubleQuotes && posOfEndSqBracket == -1) {
            if (mods == null) {
              val (a, b) = quoteMarks
              sb.insert(outerTagStart, a)
              sb.append(b)
            } else {
              if (!plaintext) {
                sb.insert(outerTagStart, "<span"+mkMods(mods)+">")
                sb.append("</span>")
              }
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




trait Typography {
  def apply(text: String, plaintext: Boolean): String
  def doubleQuoteMarks: (String, String)
  def singleQuoteMarks: (String, String)
}

object NoTypography extends Typography {
  def apply(text: String, plaintext: Boolean) = text
  val doubleQuoteMarks = ("\"", "\"")
  val singleQuoteMarks = ("'",  "'")
}

class CzechTypography(hyphenator: Hyphenator = NoHyphenator) extends Typography {
  def apply(text: String, plaintext: Boolean) = {
    var t = text
    if (!plaintext) { t = handleHyphens(t) }
    for ((search, replace) <- replacements) { t = t.replace(search, replace) }
    t
  }

  val doubleQuoteMarks = ("„", "“")
  val singleQuoteMarks = ("‚", "‘")
  //"» «", "› ‹"),

  val replacements = Seq(
    "---" -> "—", // &mdash;
    "--"  -> "–", // &ndash;
    "..." -> "…",
    "->"  -> "→",
    "<-"  -> "←",
    "<->" -> "↔",
    "(TM)"-> "™",
    "(R)" -> "®",
    "(C)" -> "©",
    " - " -> " – ", // &ndash;
    "\n- "-> " – ", // &ndash;
    " -\n"-> " – ", // &ndash;
  )

  val preposChars = "ksvzouiaKSVZOUIA"
  val preposCharsMax = preposChars.max.toInt
  val preposArr = Array.tabulate[Boolean](preposCharsMax+1) { i => preposChars.indexOf(i) != -1 }
  def isPreposChar(ch: Char) = ch <= preposCharsMax && preposArr(ch)
  def ws(ch: Char) = (ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t')

  def handleHyphens(txt: String) = {
    val sb = new StringBuilder

    var i = 0; while (i < txt.length) {

      // non word
      val start = i
      while (i < txt.length && !isAlphabetic(txt.charAt(i))) {
        i += 1
      }

      sb.append(txt, start, i)

      // word
      val wordStart = i
      while (i < txt.length && isAlphabetic(txt.charAt(i))) {
        i += 1
      }

      // at this point i points behind the word
      val wordLen = i - wordStart

      if (wordLen == 1) {
        if (i == 1 || (i < txt.length && ws(txt.charAt(i-2)) && isPreposChar(txt.charAt(i-1)) && ws(txt.charAt(i)))) {
          sb.append(txt, wordStart, i).append("\u00A0")
          i += 1 // skip following space
        } else {
          sb.append(txt, wordStart, i)
        }
      } else if (wordLen < 7) { // short words, do not hyphenate
        sb.append(txt, wordStart, i)
      } else {
        sb.append(hyphenator(txt.substring(wordStart, i), "\u00AD", 2, 3))
      }

    }

    sb.toString
  }
}
