package asciiblog

import scala.util.matching.Regex
import java.io.File
import scala.collection.mutable

import java.io.StringWriter;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;

object XMLSW {
  import java.lang.StringBuilder
  private lazy val f = XMLOutputFactory.newInstance

  def document(encoding: String = "utf-8", version: String = "1.0")(body: XMLSW => Unit) = {
    val out = new StringWriter()
    val w = f.createXMLStreamWriter(out)
    val ww = new XMLSW(w)
    ww.document(encoding, version)(body)
    w.flush()
    w.close()
    out.toString
  }

  object Simple {
    def document(body: XMLSW.Simple => Unit, sb: StringBuilder): StringBuilder = {
      new Simple(sb).document(body)
      sb
    }

    def document(body: XMLSW.Simple => Unit): StringBuilder =
      document(body, new StringBuilder)
  }

  class Simple(sb: StringBuilder) {
    def document(body: XMLSW.Simple => Unit) = {
      sb.append("<?xml version=\"1.0\" encoding=\"utf-8\"?>")
      body(this)
    }

    def element(localName: String, content: String): Unit =
      element(localName, Seq.empty, content)

    def element(localName: String)(body: XMLSW.Simple => Unit): Unit =
      element(localName, Seq.empty)(body)

    def element(localName: String, attributes: Seq[(String, String)])(body: XMLSW.Simple => Unit): Unit = {
      startElem(localName, attributes)
      body(this)
      endElem(localName)
    }

    def element(localName: String, attributes: Seq[(String, String)], content: String): Unit = {
      startElem(localName, attributes)
      txt(content)
      endElem(localName)
    }

    private def startElem(localName: String, attributes: Seq[(String, String)]) = {
      sb.append("<").append(localName)
      for ((k, v) <- attributes) {
        sb.append(" ").append(k).append("=\"")
        txt(v, true)
        sb.append("\"")
      }
      sb.append(">")
    }

    private def endElem(localName: String) = {
      sb.append("</").append(localName).append(">")
    }

    private def txt(txt: String, escapeDoubleQuotes: Boolean = false) = {
      var start = 0
      var i = 0; while (i < txt.length) {
        (txt.charAt(i): @annotation.switch) match {
          case '&' =>
            sb.append(txt, start, i)
            sb.append("&amp;")
            start = i+1
          case '>' =>
            sb.append(txt, start, i)
            sb.append("&gt;")
            start = i+1
          case '<' =>
            sb.append(txt, start, i)
            sb.append("&lt;")
            start = i+1
          case '"' => if (escapeDoubleQuotes) {
            sb.append(txt, start, i)
            sb.append("&qt;")
            start = i+1
          }
          case _ =>
        }

        i += 1
      }
      sb.append(txt, start, txt.length)
    }
  }

}


class XMLSW(val w: XMLStreamWriter) {
  def document(encoding: String, version: String)(body: XMLSW => Unit) = {
    w.writeStartDocument(encoding, version)
    body(this)
    w.writeEndDocument()
  }

  def element(localName: String, content: String) = {
    w.writeStartElement(localName)
    w.writeCharacters(content)
    w.writeEndElement()
  }

  def element(localName: String, attributes: Seq[(String, String)], content: String) = {
    w.writeStartElement(localName)
    for ((k, v) <- attributes) {
      attribute(k, v)
    }
    w.writeCharacters(content)
    w.writeEndElement()
  }

  def element(localName: String)(body: XMLSW => Unit) = {
    w.writeStartElement(localName)
    body(this)
    w.writeEndElement()
  }

  def attribute(localName: String, value: String) =
    w.writeAttribute(localName, value)
}


object util {

  private val patternBracketRegex = """(?x) ^(.*?)\{(.*)\}$ """.r

  def globFiles(pattern: String): Array[File] = ((pattern match {
    case p if p.endsWith("*") =>
      val f = new File(p.init)
      if (f.isDirectory) {
        val fs = f.listFiles
        if (fs == null) Array() else fs
      } else {
        val prefix = f.getName
        val fs = f.getParentFile.listFiles
        if (fs == null) Array() else fs.filter { _.getName.startsWith(prefix) }
      }
    case patternBracketRegex(p, variants) =>
      val f = new File(p)
      if (f.isDirectory) {
        variants.split(",").map { v => new File(f, v) }
      } else {
        variants.split(",").map { v => new File(f.getParentFile, f.getName+v) }
      }
    case _ =>
      val f = new File(pattern)
      if (f.isDirectory) f.listFiles else Array(f)
    }): Array[File]).filter(f => !f.getName.startsWith("."))

  def splitByHash(l: String): (String, String) = {
    val pos = l.indexOf('#')
    if (pos >= 0) (l.substring(0, pos), l.substring(pos))
    else          (l, "")
  }

  def escape(s: String): String = {
    val sb = new StringBuilder()
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
  def apply[T](label: String)(f: => T) = {
    val s = System.nanoTime
    val r = f
    val d = System.nanoTime - s
    println(label+" "+(d/1e6)+"ms")
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

  override def toString = "Timer "+(t.sum/1e6)+"ms"
}
