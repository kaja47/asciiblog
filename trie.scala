package asciiblog

import scala.collection.mutable

case class Trie[T](terminal: Boolean, value: T, children: Array[Trie[T]], offset: Int) {
  def lookupByte(b: Int) = {
    val idx = b-offset
    if (idx >= 0 && idx < children.length) children(idx) else null
  }

  def lookup(ch: Char): Trie[T] = {
    val res = lookupByte(ch & 0xff)
    if (res != null) res.lookupByte(ch >>> 8) else null
  }

  def apply(str: String): T = {
    val node = findNode(str)
    if (node != null && node.terminal) node.value else throw new NoSuchElementException(str)
  }

  def contains(str: String): Boolean = {
    val node = findNode(str)
    node != null && node.terminal
  }


  private def findNode(str: String) = {
    var node = this
    var i = 0; while (node != null && i < str.length) {
      node = node.lookup(str.charAt(i))
      i += 1
    }
    node
  }

  def allSubstrings(str: String): mutable.Seq[T] = {
    val res = mutable.ArrayBuffer[T]()
    var node = this
    var i = 0; while (node != null && i < str.length) {
      node = node.lookup(str.charAt(i))
      if (node != null && node.terminal) res += node.value
      i += 1
    }
    res
  }
}


object Trie {
  def make[T](map: Map[String, T]): Trie[T] =
    make(map.iterator)

  def make[T](iter: Iterator[(String, T)]): Trie[T] = {

    case class TrieBuilder[T](var terminal: Boolean = false, var value: T, val children: Array[TrieBuilder[T]])
    val root = TrieBuilder[T](false, null.asInstanceOf[T], new Array(256))

    iter foreach { case (key, value) =>
      var node = root
      for (i <- 0 until key.length*2) {

        val shift = (i%2) * 8
        val pos = i/2
        val byte = (key.charAt(pos) >>> shift) & 0xff

        if (node.children(byte) == null) {
          node.children(byte) = TrieBuilder[T](false, null.asInstanceOf[T], new Array(256))
        }

        node = node.children(byte)
      }

      node.terminal = true
      node.value = value
    }

    def compress(t: TrieBuilder[T]): Trie[T] = t match {
      case null => null
      case TrieBuilder(terminal, value, children) =>
        val firstNonNulPos = children.indexWhere(_ != null)
        val lastNonNulPos  = children.lastIndexWhere(_ != null)
        Trie(terminal, value, children.slice(firstNonNulPos, lastNonNulPos+1).map(compress), firstNonNulPos)
    }

    compress(root)
  }
}
