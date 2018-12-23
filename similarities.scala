package asciiblog

import MakeFiles.invert
import util.intersectionSize
import java.{ lang => jl }

class Similarities(articles: Seq[Article]) {
  private val arts: Array[Article] = articles.toArray
  private val tags: Map[Tag, Article] = arts.iterator.collect { case a if a.isTag => (a.asTag, a) }.toMap
  private val slugMap: Map[Slug, Int] = arts.iterator.zipWithIndex.map { case (k, v) => (k.asSlug, v) }.toMap
  private val tm: Map[Tag, Array[Int]] = {
    val tagMap = invert(arts.map { a => (a, (a.tags.visible ++ a.tags.hidden).distinct) })
    tagMap.map { case (t, as) =>
      val idxs = as.map(a => slugMap(a.asSlug)).toArray
      java.util.Arrays.sort(idxs)
      (t, idxs)
    }
  }
  private val tagsToRecommend: Seq[(Tag, Array[Int])] = {
    // do not recommend tags that
    // - are used only as hidden tags
    // - are associated with only one article
    val visibleTags = arts.iterator.flatMap(a => a.tags.visible).toSet
    tm.iterator.filter { case (t, as) => visibleTags.contains(t) && as.length > 1 }.toVector
  }
  private val reverseRels: Map[Slug, Seq[Slug]] =
    invert(articles.collect { case a if a.rel.nonEmpty => (a.asSlug, a.rel.map(Slug)) })

  def similarByTags(a: Article, count: Int, without: Seq[Article]): Seq[Article] = {
    def dateDiff(a: Article, b: Article): Int = {
      val ta = if (a.date == null) 0 else (a.date.getTime/(1024*1024)).toInt
      val tb = if (b.date == null) 0 else (b.date.getTime/(1024*1024)).toInt
      Math.abs(ta - tb)
    }

    val arrs = (a.tags.visible ++ a.tags.hidden).map(tm)
    if (arrs.isEmpty && a.rel.isEmpty) return Seq()

    val freq = new Array[Int](arts.length) // article idx -> count
    var minIdx = arts.length
    var maxIdx = 0
    for (arr <- arrs) {
      var i = 0; while (i < arr.length) {
        freq(arr(i)) += 1
        i += 1
      }
      minIdx = Math.min(minIdx, arr(0))
      maxIdx = Math.max(maxIdx, arr(arr.length-1))
    }

    for (id <- a.rel) {
      val i = slugMap(Slug(id))
      freq(i) += 64
      minIdx = Math.min(minIdx, i)
      maxIdx = Math.max(maxIdx, i)
    }
    for (id <- reverseRels.getOrElse(a.asSlug, Seq())) {
      val i = slugMap(id)
      freq(i) += 1
      minIdx = Math.min(minIdx, i)
      maxIdx = Math.max(maxIdx, i)
    }

    for (a <- without ; i <- slugMap.get(a.asSlug)) freq(i) = 0
    for (i <- slugMap.get(a.asSlug)) freq(i) = 0

    case class Key(commonTags: Int, dateDiff: Int, idx: Int)
    val topk = new TopK[Key](count)(new Ordering[Key]{
      def compare(a: Key, b: Key): Int = {
        val c1 = jl.Integer.compare(a.commonTags, b.commonTags)
        if (c1 != 0) return c1
        val c2 = jl.Integer.compare(b.dateDiff, a.dateDiff)
        if (c2 != 0) return c2
        jl.Integer.compare(a.idx, b.idx)
      }
    })

    var twoPlus = 0
    var j = minIdx; while (j < maxIdx) {
      if (freq(j) >= 2) { twoPlus += 1 }
      j += 1
    }
    val min = if (twoPlus >= count) 2 else 1

    var i = minIdx; while (i < maxIdx) {
      if (freq(i) >= min) {
        // articles with most tags in common, published closest together
        topk.add(Key(freq(i), dateDiff(a, arts(i)), i))
      }
      i += 1
    }
    topk.getAll.map(key => arts(key.idx))
  }

  def sortBySimilarity(bs: Seq[Article], a: Article): Seq[Article] = {
    val atags = a.tags.visible.toSet
    def timeOf(a: Article) = if (a.date != null) a.date.getTime else 0
    def commonTags(b: Article) = b.tags.visible.count { bt => atags.contains(bt) }
    bs.map { b => (b, (~commonTags(b), ~timeOf(b), b.slug)) }.sortBy(_._2).map(_._1)
  }

  def similarTags(t: Tag, count: Int): Seq[Article] = {
    if (!tm.contains(t)) return Seq()

    case class Key(sim: Double, tag: Tag)
    val topk = new TopK[Key](count)((a: Key, b: Key) => {
      val c1 = jl.Double.compare(a.sim, b.sim)
      if (c1 != 0) c1 else a.tag.title.compareTo(b.tag.title)
    })

    val idxs = tm(t)
    val fst = idxs(0)
    val lst = idxs(idxs.length-1)

    for ((t2, idxs2) <- tagsToRecommend if t2 != t) {
      if (!(fst > idxs2(idxs2.length-1) || lst < idxs2(0))) { // overlap check
        val maxSim = 1.0 * Math.min(idxs.length, idxs2.length) / Math.max(idxs.length, idxs2.length)
        if (topk.size < count || maxSim >= topk.head.sim) {
          val in = intersectionSize(idxs, idxs2)
          val un = idxs.size + idxs2.size - in
          val sim = in.toDouble / un
          if (sim > 0) {
            topk.add(Key(sim, t2))
          }
        }
      }
    }

    topk.getAll.map { p => tags(p.tag) }
  }
}
