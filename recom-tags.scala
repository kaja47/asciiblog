package asciiblog

import scala.collection.{ mutable, immutable }
import MakeFiles.timer

object RecommendTags {

  def apply(base: Base, slug: String) = timer ("tags") {

    val allTags: Set[Tag] = base.tagMap.keySet
    val article = base.find(slug).get

    def tagsInText(a: Article, allTags: Set[Tag]): Set[Tag] = {
      val text = a.rawText.toLowerCase
      allTags.filter { t => 
        val tl = t.title.toLowerCase
        val maxTrim = (tl.length-4) max 0 min 2
        (0 to maxTrim).map(n => tl.dropRight(n)).exists(tl => text.contains(tl))
      }
    }

    def exclude(tags: Seq[Tag]): Seq[Tag] = tags.diff(article.tags.visible)

    val tagCandidatesInText = tagsInText(article, allTags).toSeq

    val cotagFreq = mutable.Map[Tag, mutable.Map[Tag, Int]]()
    val currentSet = article.tags.visible.toSet
    for {
      a <- base.articles
      i <- 0 until a.tags.visible.length
      t1 = a.tags.visible(i)
      j <- 1 until a.tags.visible.length
      t2 = a.tags.visible(j)
    } {
      if (currentSet(t1) || currentSet(t2)) {
        cotagFreq.getOrElseUpdate(t1, mutable.Map() withDefaultValue 0)(t2) += 1
        cotagFreq.getOrElseUpdate(t2, mutable.Map() withDefaultValue 0)(t1) += 1
      }
    }

    val cotags: Seq[Tag] = article.tags.visible
      .flatMap(cotagFreq)
      .groupBy(_._1).mapValues(_.map(_._2).sum)
      .toSeq.sortBy(~_._2).map(_._1)

    println(article.title)
    println("current tags:  "+article.tags.visible.map(_.hashTag).mkString(" "))
    println("in text:       "+exclude(tagCandidatesInText).map(_.hashTag).mkString(" "))
    println("expand:        "+exclude(cotags).map(_.hashTag).mkString(" "))
  }


}
