package asciiblog

import scala.collection.mutable

object CssMinimizer {
  sealed trait CSS
  case class Rule(selectors: Seq[String], styles: Seq[(String, String)]) extends CSS
  case class Media(media: String, rules: Seq[CSS]) extends CSS

  sealed trait xCSS
  case class xRule(selectors: Seq[String], splitSelectors: Seq[Seq[String]], styles: String) extends xCSS
  case class xMedia(media: String, rules: Seq[xCSS]) extends xCSS

  def parseCSS(cssString: String): Seq[CSS] = {
    import com.steadystate.css.parser._
    import com.steadystate.css.dom._
    import com.steadystate.css.format.CSSFormat
    import org.w3c.css.sac.InputSource
    import org.w3c.dom.css._
    import java.io.StringReader
    import java.lang.StringBuilder

    val source = new InputSource(new StringReader(cssString))
    val parser = new CSSOMParser(new SACParserCSS3)
    val rules = parser.parseStyleSheet(source, null, null).getCssRules

    val fmt = new CSSFormat().setRgbAsHex(true).setUseSourceStringValues(true)

    def toLocalRepr(rules: CSSRuleList): Seq[CSS] =
      for (i <- 0 until rules.getLength) yield {
        rules.item(i) match {
          case r: CSSStyleRuleImpl =>
            val ss = r.getSelectors
            val selectors = 0 until ss.getLength map { i => ss.item(i).toString }
            val st = r.getStyle
            val styles = 0 until st.getLength map { i => st.item(i) -> minifyCssValue(st.getPropertyCSSValue(st.item(i)).asInstanceOf[CSSValueImpl].getCssText(fmt)) }

            Rule(selectors, styles)

          case r: CSSMediaRuleImpl =>
            Media(r.getMedia.asInstanceOf[MediaListImpl].getMediaText(fmt), toLocalRepr(r.getCssRules))

          case r =>
            sys.error(s"unsupported css rule $r")
        }
      }

    def minifyCssValue(value: String) = value
      .replaceAll("0(\\.\\d+em)", "$1")
      .replaceAll("#([0-9a-f])\\1([0-9a-f])\\2([0-9a-f])\\3", "#$1$2$3")

    toLocalRepr(rules)
  }

  def optimize(css: Seq[CSS]): Seq[xCSS] =
    joinStyles(collapse(css))

  private def collapse(css: Seq[CSS]): Seq[CSS] = {
    var res = Seq[CSS]()

    val sels = Set[Seq[String]]()
    for (r <- css) {
      r match {
        case r @ Rule(selectors, styles) =>
          val idx = res.indexWhere { case Rule(`selectors`, _) => true ; case _ => false }
          if (idx >= 0) {
            val old = res(idx).asInstanceOf[Rule]
            res = res.patch(idx, Seq(), 1)
            res :+= merge(old, r)
          } else {
            res :+= r
          }

        case r => res :+= r
      }
    }

    def merge(old: Rule, nw: Rule): Rule = {
      require(old.selectors == nw.selectors)
      val properties = mutable.Set[Seq[String]]() ++ nw.styles.map(_._1)
      Rule(old.selectors, old.styles.filter { case (p, _) => !properties.contains(p) } ++ nw.styles)
    }

    res
  }

  private val wildcard = Seq("*")
  private val unrecognizedChars = """(?x) [^\w\s\.\#] """.r
  private val selectorRegex     = """(?x) \G ( [.\#]? \w+ ) \s*""".r
  private def splitSelectors(selector: String): Seq[String] =
    if (!unrecognizedChars.findFirstIn(selector).isEmpty) wildcard else {
      selectorRegex.findAllMatchIn(selector).map { m => m.group(1) }.toVector.distinct
    }

  private def joinStyles(css: Seq[CSS]): Seq[xCSS] =
    css.map {
      case Rule(selectors, styles) => xRule(selectors, selectors.map(splitSelectors), styles.map { case (p, v) => p+":"+v }.mkString(";"))
      case Media(media, rules) => xMedia(media, joinStyles(rules))
    }

  //type CSSIdents = mutable.Set[String]

  def minimizeAndRender(css: Seq[xCSS], classesTagsIds: CSSIdents): String =
    minimizeAndRenderSB(css, classesTagsIds, new StringBuilder(64)).toString

  def minimizeAndRenderSB(css: Seq[xCSS], classesTagsIds: CSSIdents, sb: StringBuilder): StringBuilder = {
    css foreach {
      case xRule(selectors, splitSelectors, styles) => {
        var matches = false
        var i = 0; while (i < selectors.size) {
          val ss = splitSelectors(i)
          if (ss == wildcard || classesTagsIds == null || classesTagsIds.containsAll(ss)) {
            if (matches) sb.append(",")
            sb.append(selectors(i))
            matches = true
          }
          i += 1
        }

        if (matches) sb.append("{").append(styles).append("}")
      }
      case xMedia(media, rules) =>
        sb.append("@media ").append(media).append("{")
        minimizeAndRenderSB(rules, classesTagsIds, sb)
        sb.append("}")
    }
    sb
  }
}


import CssMinimizer._

class CSSMinimizeJob(optimizedStyle: Seq[xCSS]) {
  lazy val fullMinimizedStyle = minimizeAndRender(optimizedStyle, null)

  def styleFor(html: String, minimize: Boolean = true) =
    if (!minimize) fullMinimizedStyle
    else minimizeAndRender(optimizedStyle, CSSMinimizeJob.classesAndTags(html))
}


object CSSMinimizeJob {
  def apply(css: String) = new CSSMinimizeJob(optimize(parseCSS(css)))

  //private val classRegex = """(?x) class=(?: ("|')([\w\ ]+?)\1 | (\w+) )""".r
  //private val idRegex    = """(?x) id=   (?: ("|')([\w\ ]+?)\1 | (\w+) )""".r
  //private val tagRegex   = """\<([a-zA-Z]\w*?)\W""".r
  def classesAndTags(html: String) = {
    //val classes = classRegex.findAllMatchIn(html).map { m => if (m.group(2) != null) m.group(2) else m.group(3) }.flatMap(_.split("\\s+"))
    //val ids     = idRegex   .findAllMatchIn(html).map { m => if (m.group(2) != null) m.group(2) else m.group(3) }.flatMap(_.split("\\s+"))
    //val tags: Iterator[String]    = tagRegex.findAllMatchIn(html).map(_.group(1))
    //(tags ++ classes.map("."+_) ++ ids.map("#"+_)).toSet

    // following code is just a faster version of regexes above
    //val idents = mutable.Set[String]()
    val idents = new CSSIdents
    idents += "body"
    idents += "html"

    var pos = html.indexOf('<', 0)
    while (pos != -1) {
      var i = pos+1
      while (i < html.length && Character.isLetterOrDigit(html.charAt(i))) { i += 1 }
      if (i > pos+1) { idents += html.substring(pos+1, i) }
      pos = html.indexOf('<', i)
    }

    def p(prelude: String, prefix: String) = {
      var pos = html.indexOf(prelude, 0)
      while (pos != -1) {
        pos = pos+prelude.length
        var i = pos

        val q = html.charAt(i)
        if (q == '"' || q == '\'') {
          i = html.indexOf(q, i+1)
          for (c <- html.substring(pos+1, i).split(" ")) {
            idents += prefix+c
          }

        } else {
          while (i < html.length && Character.isLetterOrDigit(html.charAt(i))) { i += 1 }
          idents += prefix+html.substring(pos, i)
        }

        pos = html.indexOf(prelude, i)
      }
    }

    p("class=", ".")
    p("id=",    "#")
    idents
  }
}

class CSSIdents {
  private val arr = new Array[Long](4)

  private def add(h: Int) =
    arr((h >>> 6) % 4) |= (1 << (h % 64))

  def += (s: String) = add(s.hashCode)

  def contains(s: String) = {
    val h = s.hashCode
    (arr((h >>> 6) % 4) & (1 << (h % 64))) != 0
  }

  def containsAll(ss: Seq[String]): Boolean = {
    var i = 0; while (i < ss.size) {
      if (!contains(ss(i))) return false
      i += 1
    }
    true
  }
}
