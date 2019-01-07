package asciiblog

import scala.collection.mutable
import scala.util.matching.Regex
import java.util.regex.{ Pattern, Matcher }
import java.io.File
import java.lang.StringBuilder

class TopK[T: Ordering](count: Int) {
  private[this] val ord = implicitly[Ordering[T]].reverse
  private[this] val heap = mutable.PriorityQueue[T]()(ord)
  def add(x: T) = {
    if (heap.size < count) {
      heap.enqueue(x)
    } else if (!ord.gt(x, heap.head)) {
      heap.dequeue
      heap.enqueue(x)
    }
  }
  def getAll   = heap.dequeueAll.reverse
  def head     = heap.head
  def size     = heap.size
  def nonEmpty = heap.nonEmpty
  def isEmpty  = heap.isEmpty
}

object XMLSW {

  def document(body: XMLSW => Unit, sb: StringBuilder): StringBuilder = {
    new XMLSW(sb).document(body)
    sb
  }

  def document(body: XMLSW => Unit): StringBuilder =
    document(body, new StringBuilder)

  def element(localName: String)(body: XMLSW => Unit): StringBuilder = {
    val sb = new StringBuilder
    new XMLSW(sb).element(localName)(body)
    sb
  }
}

class XMLSW(sb: java.lang.StringBuilder, val html5: Boolean = false) {
  def builder = sb

  def document(body: XMLSW => Unit) = {
    if (html5) {
      sb.append("<!DOCTYPE html>")
    } else {
      sb.append("<?xml version=\"1.0\" encoding=\"utf-8\"?>")
    }
    body(this)
  }

  def shortElement(localName: String, attributes: Seq[(String, String)]): Unit =
    shortElem(localName, attributes)

  def element(localName: String, content: String): Unit =
    element(localName, Seq.empty, content)

  def element(localName: String)(body: XMLSW => Unit): Unit =
    element(localName, Seq.empty)(body)

  def element(localName: String, attributes: Seq[(String, String)])(body: XMLSW => Unit): Unit = {
    startElem(localName, attributes)
    body(this)
    endElem(localName)
  }

  def element(localName: String, attributes: Seq[(String, String)], content: String): Unit = {
    startElem(localName, attributes)
    writeString(content, false)
    endElem(localName)
  }

  def element2(localName: String)(attributes: XMLSW => Unit)(body: XMLSW => Unit): this.type = {
    sb.append("<").append(localName)
    attributes(this)
    sb.append(">")
    body(this)
    endElem(localName)
    this
  }

  def shortElement2(localName: String)(attributes: XMLSW => Unit): this.type = {
    sb.append("<").append(localName)
    attributes(this)
    if (html5) {
      sb.append(">")
    } else {
      sb.append("/>")
    }
    this
  }

  def attr(key: String, value: String): this.type = {
    writeAttr(key, value)
    this
  }

  def text(t: String): Unit = writeString(t, false)


  private def startElem(localName: String, attributes: Seq[(String, String)]) = {
    sb.append("<").append(localName)
    writeAttrs(attributes)
    sb.append(">")
  }

  private def endElem(localName: String) =
    sb.append("</").append(localName).append(">")

  private def shortElem(localName: String, attributes: Seq[(String, String)]) = {
    sb.append("<").append(localName)
    writeAttrs(attributes)
    sb.append("/>")
  }

  private def writeAttrs(attributes: Seq[(String, String)]) =
    for ((k, v) <- attributes) writeAttr(k, v)


  private def writeAttr(key: String, value: String) = {
    sb.append(" ").append(key).append("=")
    val q = !html5 || util.mustHTMLAttributeBeQuoted(value)
    if (q) sb.append("\"")
    writeString(value, true)
    if (q) sb.append("\"")
  }

  private def writeString(txt: String, escapeDoubleQuotes: Boolean) = {
    var start = 0
    var i = 0; while (i < txt.length) {
      (txt.charAt(i): @annotation.switch) match {
        case '&' =>
          sb.append(txt, start, i).append("&amp;")
          start = i+1
        case '>' =>
          sb.append(txt, start, i).append("&gt;")
          start = i+1
        case '<' =>
          sb.append(txt, start, i).append("&lt;")
          start = i+1
        case '"' => if (escapeDoubleQuotes) {
          sb.append(txt, start, i).append("&quot;")
          start = i+1
        }
        case _ =>
      }

      i += 1
    }
    sb.append(txt, start, txt.length)
  }
}


object util {

  def intersectionSize(a: Array[Int], b: Array[Int]): Int = {
    var size, ai, bi = 0
    while (ai < a.length && bi < b.length) {
      val av = a(ai)
      val bv = b(bi)
      size += (if (av == bv) 1 else 0)
      ai   += (if (av <= bv) 1 else 0)
      bi   += (if (av >= bv) 1 else 0)
    }
    size
  }

  private val patternBracketRegex = """(?x) ^(.*?)\{(.*)\}$ """.r
  def newFile(name: String, base: File) = {
    val f = new File(name)
    if (f.isAbsolute) f else new File(base, name)
  }

  def globFiles(pattern: String, baseDir: File): Array[File] = (pattern match {
    case p if p.endsWith("*") =>
      val f = newFile(p.init, baseDir)
      if (f.isDirectory) {
        val fs = f.listFiles
        if (fs == null) Array() else fs
      } else {
        val prefix = f.getName
        val fs = f.getParentFile.listFiles
        if (fs == null) Array() else fs.filter { _.getName.startsWith(prefix) }
      }
    case patternBracketRegex(p, variants) =>
      val f = newFile(p, baseDir)
      if (f.isDirectory) {
        variants.split(",").map { v => new File(f, v) }
      } else {
        variants.split(",").map { v => new File(f.getParentFile, f.getName+v) }
      }
    case _ =>
      val f = newFile(pattern, baseDir)
      if (f.isDirectory) f.listFiles else Array(f)
    }).toArray[File].filterNot(_.getName.startsWith("."))

  def splitByHash(l: String): (String, String) = {
    val pos = l.indexOf('#')
    if (pos >= 0) (l.substring(0, pos), l.substring(pos))
    else          (l, "")
  }

  def escape(s: String): String = {
    val sb = new StringBuilder
    var i = 0; while (i < s.length) {
      s.charAt(i) match {
        case '"' => sb append "&quot;"
        case '&' => sb append "&amp;"
        case '<' => sb append "&lt;"
        case '>' => sb append "&gt;"
        case ch  => sb append ch
      }
      i += 1
    }
    sb.toString
  }

  def quoteHTMLAttribute(attr: String): String =
    quoteHTMLAttribute(attr, new StringBuilder).toString

  def quoteHTMLAttribute(attr: String, sb: StringBuilder): StringBuilder =
    if (mustHTMLAttributeBeQuoted(attr)) sb.append("\"").append(attr).append("\"") else sb.append(attr)

  def mustHTMLAttributeBeQuoted(attr: String): Boolean = {
    val chars = " \t\n\r\f\"'`=<>"
    var i = 0; while (i < attr.length) {
      if (chars.indexOf(attr.charAt(i)) != -1) {
        return true
      }
      i += 1
    }
    false
  }

  def splitByRepeating[T](xs: Seq[T], t: T): Seq[Seq[T]] = {
    var i = 0

    Iterator.continually {
      while (i < xs.length && xs(i) == t) { i += 1 }

      if (i >= xs.length) null else {
        val res = mutable.ArrayBuffer[T]()

        while (i < xs.length && xs(i) != t) {
          res += xs(i)
          i += 1
        }

        res
      }
    }.takeWhile(_ != null).toVector
  }

  def splitByInterval[T](xs: Seq[T], begin: T, end: T): Iterator[Seq[T]] =
    splitByInterval(xs, (_: T) == begin, (_: T) == end)

  def splitByInterval[T](xs: Seq[T], begin: T => Boolean, end: T => Boolean): Iterator[Seq[T]] = {
    var i = 0
    var interval = false

    Iterator.continually {
      if (i >= xs.length) {
        null

      } else if (!interval) {
        val res = mutable.ArrayBuffer[T]()
        while (i < xs.length && !begin(xs(i))) { res += xs(i); i += 1 }
        interval = true
        res

      } else {
        val res = mutable.ArrayBuffer[T]()
        while (i < xs.length && !end(xs(i))) { res += xs(i); i += 1 }
        if (i < xs.length && end(xs(i))) { res += xs(i); i += 1 }
        interval = false
        res
      }

    }.takeWhile(_ != null).filter(_.nonEmpty)
  }
}

object timer {
  def apply[T](label: String, blog: Blog = null)(f: => T) = {
    val s = System.nanoTime
    val r = f
    val d = System.nanoTime - s
    if (blog == null || blog.printTiming) {
      println(label+" "+(d/1e6)+"ms")
    }
    r
  }
}

class Timer {
  private val t = new java.util.concurrent.atomic.LongAdder()
  private var _start = 0L

  def apply[T](f: => T) = {
    val s = System.nanoTime
    val r = f
    t.add(System.nanoTime - s)
    r
  }

  def start(): Unit = {
    if (_start != 0L) sys.error("what not yet ended cannot start again")
    _start = System.nanoTime
  }

  def end(): Unit = {
    if (_start == 0L) sys.error("what never started cannot end")
    t.add(System.nanoTime - _start)
    _start = 0L
  }

  def ms = (t.sum/1e6)+"ms"
  override def toString = "Timer "+ms
}
