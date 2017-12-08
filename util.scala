package asciiblog

import scala.util.matching.Regex

object util {
  def matches(r: Regex, s: String) = r.findFirstIn(s).nonEmpty
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
  def apply[T](f: => T) = {
    val s = System.nanoTime
    val r = f
    t.add(System.nanoTime - s)
    r
  }
  override def toString = "Timer "+(t.sum/1e6)+"ms"
}
