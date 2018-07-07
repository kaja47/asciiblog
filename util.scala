package asciiblog

import scala.util.matching.Regex
import java.io.File

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
