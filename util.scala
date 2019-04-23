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
  def getAll: Seq[T] = heap.dequeueAll.reverse
  def head     = heap.head
  def size     = heap.size
  def nonEmpty = heap.nonEmpty
  def isEmpty  = heap.isEmpty
}


class LongTopK(count: Int) {
  protected var arr = new Array[Long](count)
  // top points behind the last element
  protected var top = 0
  protected var min = Long.MinValue

  def size = top
  def isEmpty = top == (0)
  def nonEmpty = top != (0)

  def head = {
    if (top == 0) throw new NoSuchElementException
    arr(0)
  }

  /** returns value that was deleted or Int.MinValue */
  def add(x: Long): Unit = {
    if (size < count) {
      _insert(x)
      min = arr(0)

    } else if (x > min) {
      _deleteMinAndInsert(x)
      min = arr(0)

    }
  }

  def getAll(): Array[Long] = {
    val res = new Array[Long](size)
    var i = res.length-1 ; while (i >= 0) {
      res(i) = arr(0)
      _deleteMin()
      i -= 1
    }
    res
  }

  private def _insert(x: Long) = {
    arr(top) = x
    swim(top)
    top += 1
  }

  private def _deleteMin() = {
    if (top == 0) throw new NoSuchElementException("underflow")
    top -= 1
    swap(0, top)
    arr(top) = 0
    sink(0)
  }

  private def _deleteMinAndInsert(x: Long) = {
    if (top == 0) throw new NoSuchElementException("underflow")
    arr(0) = x
    sink(0)
  }

  private def swap(a: Int, b: Int) = {
    val tmp = arr(a)
    arr(a) = arr(b)
    arr(b) = tmp
  }

  private def parent(pos: Int) = (pos - 1) / 2
  private def child(pos: Int) = pos * 2 + 1

  // moves value at the given position up towards the root
  private def swim(_pos: Int): Unit = {
    var pos = _pos
    while (pos > 0 && arr(parent(pos)) > arr(pos)) {
      swap(parent(pos), pos)
      pos = parent(pos)
    }
  }

  // moves value at the given position down towards leaves
  private def sink(_pos: Int): Unit = {
    val key = arr(_pos)
    var pos = _pos
    while (child(pos) < top) {
      var ch = child(pos)
      if ((ch+1) < top && arr(ch+1) < arr(ch)) ch += 1
      if (key <= arr(ch)) {
        arr(pos) = key
        return
      }
      arr(pos) = arr(ch)
      pos = ch
    }
    arr(pos) = key
  }
}


object XMLSW {

  def document(body: XMLSW => Unit, sb: StringBuilder): StringBuilder = {
    new XMLSW(sb).document(body)
    sb
  }

  def document(body: XMLSW => Unit): StringBuilder =
    document(body, new StringBuilder)

  def element(localName: String, attributes: Seq[(String, String)] = Seq.empty)(body: XMLSW => Unit): StringBuilder = {
    val sb = new StringBuilder
    new XMLSW(sb).element(localName, attributes)(body)
    sb
  }

  def element(localName: String, attributes: Seq[(String, String)], body: String): StringBuilder = {
    val sb = new StringBuilder
    new XMLSW(sb).element(localName, attributes, body)
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
  import java.time.LocalDateTime

  implicit val localDateTimeOrdering =
    math.Ordering.ordered[LocalDateTime](t => t.asInstanceOf[Comparable[LocalDateTime]])

  implicit class SeqOps[T](val seq: Seq[T]) extends AnyVal {
    def groupBySorted[U: Ordering](f: T => U): Seq[(U, Seq[T])] =
      seq.groupBy(f).toSeq.sortBy(_._1)
  }

  def invert[A, B](m: Seq[(A, Seq[B])]): Map[B, Seq[A]] = {
    val res = mutable.Map[B, mutable.ArrayBuffer[A]]()
    for ((a, bs) <- m; b <- bs) { res.getOrElseUpdate(b, new mutable.ArrayBuffer[A]) += a }
    res.iterator.map { case (b, as) => (b, as.toVector) }.toMap
  }

  def invert1[A, B](m: Seq[(A, Seq[B])]): Map[B, A] = {
    val res = mutable.Map[B, A]()
    for ((a, bs) <- m; b <- bs) { res.getOrElseUpdate(b, a) }
    res.toMap
  }

  def distinctBy[T, U](xs: Seq[T])(f: T => U): Seq[T] = // TODO replace by builtin method in scala 2.13
    if (xs.length <= 1) xs else {
      val set = mutable.Set[U]()
      val b = xs.genericBuilder[T]
      for (x <- xs) {
        if (set.add(f(x))) {
          b += x
        }
      }
      b.result()
    }

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

  def escape(s: String): String =
    escape(s, 0, s.length, new StringBuilder).toString

  def escape(s: String, from: Int, to: Int, sb: StringBuilder): StringBuilder = {
    var i = from; while (i < to) {
      s.charAt(i) match {
        case '"' => sb append "&quot;"
        case '&' => sb append "&amp;"
        case '<' => sb append "&lt;"
        case '>' => sb append "&gt;"
        case ch  => sb append ch
      }
      i += 1
    }
    sb
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
