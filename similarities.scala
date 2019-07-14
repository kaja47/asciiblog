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

  private def dayDiff(a: Article, b: Article): Int = {
    val ta = if (a.date == null) 0 else (a.date.toLocalDate.toEpochDay).toInt
    val tb = if (b.date == null) 0 else (b.date.toLocalDate.toEpochDay).toInt
    Math.abs(ta - tb)
  }

  private[this] val similarities: Array[Seq[Similar]] = {
    val sims: Array[Seq[Similar]] = arts.map {
      case a if a.isTag => similarTags(a.asTag, count)
      case a            => similarArticles(a, count)
    }

    // for articles without tags add articles similar to its rel articles
    Array.tabulate(sims.length) { i =>
      arts(i) match {
        case a if !a.isTag && sims(i).size < count && a.rel.nonEmpty =>
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


  private def detectSeries: Seq[Seq[Article]] = {
    val regex = """^(\D*)(\d*).*$""".r
    def extractPrefix(a: Article) = {
      val regex(prefix, number) = a.title
      (prefix.trim, if (number == "") 0 else number.toInt, a)
    }

    def isValidRange(xs: Seq[Int]) = {
      val range = xs.max-xs.min
      val coverage = range.toDouble / xs.length
      range > 0 && coverage > 0.5 && coverage < 2
    }

    arts.toVector
      .filter(!_.isTag)
      .map(extractPrefix)
      .groupBy(_._1)
      .filter { case (prefix, articles) => prefix.nonEmpty && articles.size > 1 && !articles.forall(_._2 != 0) && isValidRange(articles.map(_._2)) }
      .map { case (_, articles) => articles.map { case (_, _, a) => a } }
      .toVector
  }

  private lazy val series: Map[Slug, Array[Slug]] =
    detectSeries.flatMap { as =>
      val set = as.map(_.asSlug).toArray
      set.map(a => (a, set))
    }.toMap


  def apply(a: Article): Seq[Similar] = similarities(slugMap(a.asSlug))


  final val TagWeight = 2
  final val RelWeight = 64
  final val ReverseRelWeight = 2
  final val SeriesWeight = 1 // this should only break ties
  final val BacklinkWeight = -1

  private def similarArticles(a: Article, count: Int): Seq[Similar] = {
    if (a.tags.isEmpty && a.rel.isEmpty) return Seq()
    val arrs = (a.tags.visible ++ a.tags.hidden).map(tagMap)

    val freq = new Array[Int](arts.length) // article idx -> count
    for (arr <- arrs) {
      var i = 0; while (i < arr.length) {
        freq(arr(i)) += TagWeight
        i += 1
      }
    }
    for (id <- a.rel) {
      freq(slugMap(Slug(id))) += RelWeight
    }
    for (id <- reverseRels.getOrElse(a.asSlug, Seq())) {
      freq(slugMap(id)) += ReverseRelWeight
    }
    for (id <- series.getOrElse(a.asSlug, Array())) {
      freq(slugMap(id)) += SeriesWeight
    }

    for (a <- a.backlinks ; i <- slugMap.get(a.asSlug)) freq(i) += BacklinkWeight
    for (i <- slugMap.get(a.asSlug)) freq(i) = 0

    val topk = new LongTopK(count)

    def pack(commonTags: Int, dateDiff: Int, idx: Int): Long = {
      require(commonTags < (1<<15) && commonTags >= 0) // 2B
      require(dateDiff < (1<<23) && dateDiff >= 0)     // 3B
      require(idx < (1<<24))                           // 3B
      commonTags.toLong << 48 | ((~dateDiff).toLong & 0xffffffL) << 24 | idx
    }
    def unpackIdx(x: Long): Int = x.toInt & ((1 << 24) - 1)
    def unpackDD(x: Long): Int = (~(x >> 24) & 0xffffffL).toInt
    def unpackCT(x: Long): Int = (x >> 48).toInt

    var i = 0; while (i < arts.length) {
      if (freq(i) >= 1) {
        // articles with most tags in common, published closest together
        topk.add(pack(freq(i), dayDiff(a, arts(i)), i))
//        val dd = dayDiff(a, arts(i))
//        val f = freq(i)
//        val p = pack(f, dd, i)
//        require(unpackIdx(p) == i)
//        require(unpackDD(p) == dd)
//        require(unpackCT(p) == f)
      }
      i += 1
    }
    topk.getAll.map { key => Similar(arts(unpackIdx(key)), unpackCT(key), unpackDD(key)) }
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
