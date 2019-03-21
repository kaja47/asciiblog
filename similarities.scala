package asciiblog

import util.{ intersectionSize, invert }
import java.{ lang => jl }

class Similarities(_articles: Seq[Article], count: Int) {
  private[this] val arts: Array[Article] = _articles.toArray
  private[this] val tags: Map[Tag, Article] = arts.iterator.collect { case a if a.isTag => (a.asTag, a) }.toMap
  private[this] val slugMap: Map[Slug, Int] = arts.iterator.zipWithIndex.map { case (k, v) => (k.asSlug, v) }.toMap
  private[this] val tagMap: Map[Tag, Array[Int]] =
    invert(arts.map { a => (a, (a.tags.visible ++ a.tags.hidden).distinct) })
      .map { case (t, as) =>
        val idxs = as.map(a => slugMap(a.asSlug)).toArray
        java.util.Arrays.sort(idxs)
        (t, idxs)
      }

  // do not recommend tags that
  // - are used only as hidden tags
  // - are associated with only one article
  private[this] val tagsToRecommend: Seq[(Tag, Array[Int])] = {
    val visibleTags = arts.iterator.flatMap(a => a.tags.visible).toSet
    tagMap.iterator.filter { case (t, as) => visibleTags.contains(t) /*&& as.length > 1*/ }.toVector
  }

  private[this] val reverseRels: Map[Slug, Seq[Slug]] =
    invert(_articles.collect { case a if a.rel.nonEmpty => (a.asSlug, a.rel.map(Slug)) })

  private def dateDiff(a: Article, b: Article): Int = {
    val ta = if (a.date == null) 0 else (a.date.toLocalDate.toEpochDay).toInt
    val tb = if (b.date == null) 0 else (b.date.toLocalDate.toEpochDay).toInt
    Math.abs(ta - tb)
  }

  private[this] val similarities: Array[Seq[Similar]] = {
    val sims = arts.map {
      case a if a.isTag => similarTags(a.asTag, count)
      case a            => similarArticles(a, count, a.backlinks)
    }

    Array.tabulate(sims.length) { i =>
      arts(i) match {
        case a if !a.isTag && a.tags.isEmpty && a.rel.nonEmpty =>
          // not terribly optimized, but it's called only few times
          val slugs = sims(i).map(_.article.slug).toSet
          val extraSims = sims(i)
            .flatMap(a => sims(slugMap(a.article.asSlug)))
            .filter(a => !slugs.contains(a.article.slug))
            .groupBy(a => a.article.slug)
            .toSeq
            .map { case (_, as) => (as.head, as.size) }
            .sortBy(~_._2)
            .take(count - sims(i).size)
            .map(_._1)

          sims(i) ++ extraSims
        case a => sims(i)
      }
    }
  }

  def apply(a: Article): Seq[Similar] = similarities(slugMap(a.asSlug))

  def similarArticles(a: Article, count: Int, without: Seq[Article]): Seq[Similar] = {
    if (a.tags.isEmpty && a.rel.isEmpty) return Seq()
    val arrs = (a.tags.visible ++ a.tags.hidden).map(tagMap)

    val freq = new Array[Int](arts.length) // article idx -> count
    for (arr <- arrs) {
      var i = 0; while (i < arr.length) {
        freq(arr(i)) += 1
        i += 1
      }
    }

    for (id <- a.rel) {
      val i = slugMap(Slug(id))
      freq(i) += 64
    }
    for (id <- reverseRels.getOrElse(a.asSlug, Seq())) {
      val i = slugMap(id)
      freq(i) += 1
    }

    for (a <- without ; i <- slugMap.get(a.asSlug)) freq(i) = 0
    for (i <- slugMap.get(a.asSlug)) freq(i) = 0

    val topk = new LongTopK(count)

    def pack(commonTags: Int, dateDiff: Int, idx: Int): Long = {
      require(commonTags < 128 && commonTags > 0) // 1B
      require(dateDiff >= 0)                      // 4B
      require(idx < (1<<24))                      // 3B
      commonTags.toLong << 56 | ((~dateDiff).toLong & 0xffffffffL) << 24 | idx
    }
    def unpack(x: Long) = x.toInt & ((1 << 24) - 1)
    def unpackCT(x: Long) = (x >> 56).toInt
    def unpackDD(x: Long) = (~(x >> 24) & 0xffffffffL).toInt

    var i = 0; while (i < arts.length) {
      if (freq(i) >= 1) {
        // articles with most tags in common, published closest together
        topk.add(pack(freq(i), dateDiff(a, arts(i)), i))
      }
      i += 1
    }
    topk.getAll.map { key => Similar(arts(unpack(key)), unpackCT(key), unpackDD(key)) }
  }


  def similarTags(t: Tag, count: Int): Seq[Similar] = {
    if (!tagMap.contains(t)) return Seq()

    case class Key(sim: Double, tag: Tag)
    val topk = new TopK[Key](count)((a: Key, b: Key) => {
      val c1 = jl.Double.compare(a.sim, b.sim)
      if (c1 != 0) c1 else a.tag.title.compareTo(b.tag.title)
    })

    val idxs = tagMap(t)
    val fst = idxs(0)
    val lst = idxs(idxs.length-1)

    val rels = tags(t).rel.map(r => arts(slugMap(Slug(r))).asTag).toSet

    for ((t2, idxs2) <- tagsToRecommend if t2 != t) {
      val inRels = rels.contains(t2)

      // overlap check
      if (!(fst > idxs2(idxs2.length-1) || lst < idxs2(0)) || inRels) {
        // maximum possible similarity
        val maxSim = 1.0 * Math.min(idxs.length, idxs2.length) / Math.max(idxs.length, idxs2.length)
        if (topk.size < count || maxSim >= topk.head.sim || inRels) {
          val in = intersectionSize(idxs, idxs2)
          val un = idxs.size + idxs2.size - in
          val sim = in.toDouble / un + (if (inRels) 0.5 else 0)
          if (sim > 0) {
            topk.add(Key(sim, t2))
          }
        }
      }
    }

    topk.getAll.map { p => Similar(tags(p.tag), p.sim, 0) }
  }
}
