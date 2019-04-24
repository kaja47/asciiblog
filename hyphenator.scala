package asciiblog

import java.lang.StringBuilder
import scala.collection.mutable

/**
 * http://www.tug.org/docs/liang/
 */
class Hyphenator(patternFile: String, exceptionFile: String = null, val separator: String = "-", encoding: String) {

  private def read(file: String, f: String => (String, Array[Int])): Map[String, Array[Int]] =
    if (file == null) Map()
    else io.Source.fromFile(file, encoding).getLines.map(f).toMap

  private val patterns   = read(patternFile, mkPattern)
  private val exceptions = read(exceptionFile, mkException)
  private val patternsTrie = Trie.make[Array[Int]](patterns.iterator)

  /**
   * even - unacceptable location
   * odd  - acceptable location
   * higher numbers have higher priority
   */
  private def mkPattern(pattern: String) = {
    val pat     = pattern.replaceAll("[0-9]", "")
    val numbers = pattern.split("[^0-9]")
    val points  = Array.tabulate(numbers.length){ i => if (numbers(i) == "") 0 else numbers(i).toInt }
    (pat, points)
  }

  private def mkException(exception: String) = {
    val word = exception.replace(separator, "")
    val points = new Array[Int](word.length + 1)
    points(0) = 0

    for (i <- 0 until word.length) {
      if (separator == exception.charAt(i)) {
        points(i+1) = 1
      } else {
        points(i+1) = 0
      }
    }

    (word, points)
  }

  def apply(word: String, hypen: String = "-", front: Int = 2, back: Int = 2): String = {
    if (word.length < front+back) {
      return word
    }

    val points: Array[Int] =
      if (exceptions.contains(word)) {
        exceptions(word)

      } else {
        val dottedWord = "."+word.toLowerCase+"."
        val points0 = new Array[Int](dottedWord.length + 1)

        for (i <- 0 until dottedWord.length) {
          var node = patternsTrie
          var j = i
          while (node != null && j < dottedWord.length) {
            node = node.lookup(dottedWord.charAt(j))
            if (node != null && node.terminal) {
              val tpoints = node.value
              for (l <- 0 until tpoints.length) {
                points0(i + l) = Math.max(points0(i + l), tpoints(l))
              }
            }
            j += 1
          }
        }

        points0
      }

    points(0) = 0
    points(1) = 0
    points(2) = 0
    points(points.length-1) = 0
    points(points.length-2) = 0
    points(points.length-3) = 0

    val res = new StringBuilder(word.length*2)
    val len = points.length - 3
    for (i <- 0 until len) {
      res.append(word.charAt(i))
      if (i >= front-1 && i < len-back && (points(i + 2) % 2) == 1) {
        res.append(hypen)
      }
    }

    res.toString
  }

}
