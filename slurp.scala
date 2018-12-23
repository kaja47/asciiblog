package asciiblog

import java.lang.StringBuilder

object Slurp {
  type Replacement = (Slurp, StringBuilder) => Unit

  def apply(s: String, groups: Int = 0) = new Slurp(s, 0, s.length, groups)
  def apply(f: Slurp => Unit) = f
}

trait Cursor {
  def moveNext(): Boolean
}

class Slurp(val s: String, from: Int, to: Int, groups: Int = 0) { self =>
  require(from >= 0 && from <= s.length, s"from: $from >= 0 && from <= ${s.length}")
  require(to >= 0 && to <= s.length,     s"to: $to >= 0 && to <= ${s.length}")
  require(groups >= 0,                   s"groups must be non-negative, $groups given")

  private[asciiblog] var pos = from
  private[asciiblog] var mark = from // mark >= pos must hold at any time
  private[asciiblog] var _match = true
  private[asciiblog] var _groups: Array[Int] = null

  if (groups > 0) {
    reserveGroups(groups)
  }

  override def toString = s"Slurp(pos=$pos, mark=$mark, s.length=${s.length})"

  private final val MAX = Int.MaxValue / 2

  def reserveGroups(n: Int): this.type = {
    _groups = new Array[Int](n*2)
    this
  }

  def isEnd = pos >= to
  def touchesEnd = mark > to
  def matchLength = mark - pos

  def matches = _match
  def reset() = {
    pos = from
    mark = from
    _match = true
    this
  }
  def unmatch() = {
    mark = pos
    _match = true
    this
  }
  def tryAgain() = {
    _match = true
    this
  }
  def mustMatch() = {
    if (!_match) sys.error("match error")
    this
  }

  def copy = {
    val n = new Slurp(s, from, to)
    n.pos = pos
    n.mark = mark
    n._match = _match
    n
  }
  def view = new Slurp(s, pos, mark)
  def asView() = {
    val v = view
    ignore()
    v
  }

  def canMatch(f: Slurp => Unit) = {
    val x = copy
    f(x)
    x._match
  }

  def atomically(f: Slurp => Unit) = {
    val m = mark
    val p = pos
    f(self)
    if (!_match) {
      pos  = p
      mark = m
    }
  }

  // matchers

  def runWhile(f: Char => Boolean, min: Int, max: Int): this.type = {
    if (!_match) return this
    val end = Math.min(to, mark + max)
    var i = mark; while (i < end && f(s.charAt(i))) {
      i += 1
    }
    _match &= (i - mark >= min)
    if (_match) mark = i
    this
  }

  def oneChar(ch: Char): this.type = {
    val ok = _match && mark < to && s.charAt(mark) == ch
    mark += (if (ok) 1 else 0)
    _match = ok
    this
  }

  def runChar(ch: Char, min: Int, max: Int): this.type = {
    if (!_match) return this
    val end = Math.min(to, mark + max)
    var i = mark; while (i < end && s.charAt(i) == ch) {
      i += 1
    }
    _match &= (i - mark >= min)
    if (_match) mark = i
    this
  }

  def runChars(chars: String, min: Int, max: Int): this.type = {
    if (!_match) return this
    val end = Math.min(to, mark + max)
    var i = mark; while (i < end && chars.indexOf(s.charAt(i)) != -1) {
      i += 1
    }
    _match &= (i - mark >= min)
    if (_match) mark = i
    this
  }

  def any(min: Int = 0, max: Int = MAX) = runWhile(_ => true, min, max)
  def whitespaces(min: Int = 0, max: Int = MAX) = runWhile(Character.isWhitespace, min, max)

  def spaces(min: Int = 0, max: Int = MAX) = runChar(' ', min, max)
  def chars(ch: Char, min: Int = 0, max: Int = MAX) = runChar(ch, min, max)
  def digits(min: Int = 0, max: Int = MAX) = runWhile(ch => ch >= '0' && ch <= '9', min, max)

  def space() = oneChar(' ')
  def char(ch: Char) = oneChar(ch)
  def digit() = digits(1, 1)

  def until(ch: Char): this.type = { //runWhile(_ != ch, 0, MAX)
    if (!_match) return this
    val i = s.indexOf(ch, mark)
    if (i == -1 || i > to) {
      mark = to
    } else {
      mark = i
    }
    this
  }
  def until(chars: String) = runWhile(c => chars.indexOf(c) == -1, 0, MAX)
  def untilStr(str: String): this.type = {
    if (!_match) return this
    val i = s.indexOf(str, mark)
    if (i == -1 || i > to) {
      mark = to
    } else {
      mark = i
    }
    this
  }

  def to(ch: Char) = until(ch).char(ch)
  def toStr(str: String): this.type = {
    if (!_match) return this
    val i = s.indexOf(str, mark)
    if (i == -1 || i+str.length > to) {
      mark = to
      _match = false
    } else {
      mark = i + str.length
    }
    this
  }

  def delimited(begin: Char, end: Char) = char(begin).to(end)
  def quoted(delim: Char): this.type = quoted(delim, delim)
  def quoted(begin: Char, end: Char): this.type = {
    char(begin)
    var i = mark
    while (i < to && (s.charAt(i) != end || s.charAt(i-1) == '\\')) {
      i += 1
    }
    mark = i + 1
    this
  }

  // extractors
  
  def asGroup(gr: Int, startOff: Int = 0, endOff: Int = 0) = {
    _groups(gr*2)   = pos+startOff
    _groups(gr*2+1) = mark+endOff
    pos = mark
    this
  }
  def groupStart(gr: Int, off: Int = 0) = {
    _groups(gr*2) = pos+off
    this
  }
  def groupEnd(gr: Int, off: Int = 0) = {
    _groups(gr*2+1) = mark+off
    this
  }
  def group(gr: Int) = {
    val x = copy
    x.pos  = _groups(gr*2)
    x.mark = _groups(gr*2+1)
    x
  }

  def asString(startOff: Int = 0, endOff: Int = 0) = {
    val res = s.substring(pos+startOff, mark+endOff)
    pos = mark
    res
  }

  def asInt() = {
    var res = 0
    var i = pos
    while (i < mark) {
      res = res * 10 + s.charAt(i) - '0'
      i += 1
    }
    pos = mark
    res
  }

  def ignore(n: Int = 0) = { mark += n; pos = mark; this }
  def ! = ignore()

  def skip(n: Int) = {
    mark += n
    this
  }

  // more elaborate constructs

  def cursor(delimiter: Slurp => Unit, move: Slurp => Unit) = new Cursor {
    def moveNext(): Boolean = {
      while (!isEnd) {
        val m = mark
        delimiter(self)
        ignore().tryAgain()
        val p = pos
        atomically(move)
        if (_match) return { pos = p; true }
        if (mark == m) { skip(1) }
        tryAgain()
      }
      false
    }
  }

  // Iterator.continually{ c.moveNext(); self }.takeWhile(!_.isEnd)
  def iterator(delimiter: Slurp => Unit, move: Slurp => Unit) = {
    val c = cursor(delimiter, move)
    new Iterator[Slurp] {
      private var defined = false
      def hasNext = defined || { defined = c.moveNext(); defined }
      def next() = if (hasNext) { defined = false; self } else Iterator.empty.next()
    }
  }

  def replace(delimiter: Slurp => Unit, move: Slurp => Unit, f: Slurp.Replacement): StringBuilder = {
    val out = new StringBuilder

    val c = cursor(delimiter, move)
    var lastPos = 0

    while (c.moveNext()) {
      out.append(s, lastPos, pos)
      f(this, out)
      lastPos = mark
    }
    out.append(s, lastPos, s.length)
  }

  def appendInto(sb: StringBuilder, startOff: Int, endOff: Int) =
    sb.append(s, pos+startOff, mark+endOff)

}
