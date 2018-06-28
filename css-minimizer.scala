package asciiblog


/**
 * usage:
 * val st1 = parseCSS("a { color: red; }")
 * val st2 = parseCSS("a { color: black; }")
 * render(minimize(optimize(st1 ++ st2), classesTagsIds))
 */
object CssMinimizer {
  import com.steadystate.css.parser._ 
  import com.steadystate.css.dom._
  import com.steadystate.css.format.CSSFormat
  import org.w3c.css.sac.InputSource
  import org.w3c.dom.css._
  import java.io.StringReader
  import scala.collection.mutable

  sealed trait CSS
  case class Rule(selectors: Seq[String], styles: Seq[(String, String)]) extends CSS
  case class Media(media: String, rules: Seq[CSS]) extends CSS

  sealed trait xCSS
  case class xRule(selectors: Seq[String], splitSelectors: Seq[Set[String]], styles: String) extends xCSS
  case class xMedia(media: String, rules: Seq[xCSS]) extends xCSS

  def parseCSS(cssString: String): Seq[CSS] = {
    val source = new InputSource(new StringReader(cssString))
    val parser = new CSSOMParser(new SACParserCSS3)
    val rules = parser.parseStyleSheet(source, null, null).getCssRules

    val fmt = new CSSFormat().setRgbAsHex(true).setUseSourceStringValues(true)

    def toLocalRepr(rs: CSSRuleList): Seq[CSS] =
      for (i <- 0 until rs.getLength) yield {
        rs.item(i) match {
          case r: CSSStyleRuleImpl =>
            val ss = r.getSelectors
            val selectors = 0 until ss.getLength map { i => ss.item(i).toString }
            val st = r.getStyle
            val styles = 0 until st.getLength map { i => st.item(i) -> st.getPropertyCSSValue(st.item(i)).asInstanceOf[CSSValueImpl].getCssText(fmt) }
            Rule(selectors, styles)

          case r: CSSMediaRuleImpl =>
            Media(r.getMedia.asInstanceOf[MediaListImpl].getMediaText(fmt), toLocalRepr(r.getCssRules))

          case r =>
            sys.error(s"unsupported css rule $r")
        }
      }

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

  private val wildcard = Set("*")
  private val unrecognizedChars = """(?x) [^\w\s\.\#] """.r
  private val selectorRegex     = """(?x) \G ( [.\#]? \w+ ) \s*""".r
  private def splitSelectors(selector: String): Set[String] =
    if (!unrecognizedChars.findFirstIn(selector).isEmpty) wildcard else {
      selectorRegex.findAllMatchIn(selector).map { m => m.group(1) }.toSet
    }

  private def joinStyles(css: Seq[CSS]): Seq[xCSS] = 
    css.map {
      case Rule(selectors, styles) => xRule(selectors, selectors.map(splitSelectors), styles.map { case (p, v) => p+":"+v }.mkString(";"))
      case Media(media, rules) => xMedia(media, joinStyles(rules))
    }

  def minimize(css: Seq[xCSS], classesTagsIds: Set[String]): Seq[xCSS] =
    css.flatMap {
      case xRule(selector, splitSelectors, styles) =>
        val activeSelectors = selector zip splitSelectors filter { case (s, ss) => ss == wildcard || ss.forall(classesTagsIds.contains) } map (_._1)
        if (activeSelectors.isEmpty) None else Some(xRule(activeSelectors, null, styles))
      case xMedia(media, rules) => 
        val cs = minimize(rules, classesTagsIds)
        if (cs.isEmpty) None else Some(xMedia(media, cs))
    }

  def render(css: Seq[xCSS]): String =
    css.map {
      case xRule(selector, _, styles) => selector.mkString(",")+"{"+styles+"}"
      case xMedia(media, rules) => "@media "+media+"{"+render(rules)+"}"
    }.mkString("\n")
}