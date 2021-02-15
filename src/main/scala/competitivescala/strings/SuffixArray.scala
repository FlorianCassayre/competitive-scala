package competitivescala.strings

object SuffixArray {

  // Complexity: O(n log n)
  // Requires a injection between T and Ints
  def sortSuffixes[T](seq: IndexedSeq[T])(implicit ord: Ordering[T], toInt: T => Int): IndexedSeq[Int] = {
    val n = seq.size
    val indices = seq.indices
    def iteratePow(k: Int, array: IndexedSeq[Int], ranks: IndexedSeq[Int]): IndexedSeq[Int] = {
      if(k < n) {
        val hk = k / 2
        val newRanks = indices.foldLeft(ranks)((currentRanks, i) =>
          currentRanks.updated(array(i),
            if(i > 0 &&
              ranks(array(i - 1)) == ranks(array(i)) &&
              array(i - 1) + k < n &&
              ranks(array(i - 1) + hk) == ranks(array(i) + hk))
              currentRanks(array(i - 1))
            else
              i
          )
        )
        val (newArray, _) = indices.foldLeft((array, indices.toVector)) { case ((currentArray, currentCount), i) =>
          val s = array(i) - k
          if(s >= 0) {
            val r = newRanks(s)
            (currentArray.updated(currentCount(r), s), currentCount.updated(r, currentCount(r) + 1))
          } else {
            (currentArray, currentCount)
          }
        }
        iteratePow(k * 2, newArray, newRanks)
      } else {
        array
      }
    }
    iteratePow(1, seq.indices.reverse.sortBy(seq), seq.map(toInt))
  }

  // Complexity: O(n log n)
  def burrowsWheelerTransform[T](seq: IndexedSeq[T])(implicit ord: Ordering[T], toInt: T => Int): (IndexedSeq[T], IndexedSeq[T]) = {
    val n = seq.size
    val sorted = sortSuffixes(seq)
    val plusLast = n +: sorted
    val index = plusLast.indexOf(0)
    (plusLast.take(index).map(i => seq(i - 1)), plusLast.drop(index + 1).map(i => seq(i - 1)))
  }

  // Complexity: O(n log n)
  def burrowsWheelerInverseTransform[T](seq1: IndexedSeq[T], seq2: IndexedSeq[T])(implicit ord: Ordering[T], toInt: T => Int): IndexedSeq[T] = {
    val all = seq1 ++ seq2
    if(all.nonEmpty) {
      def withRank(seq: Seq[T]): Seq[(T, Int)] =
        seq.foldLeft((Seq.empty[(T, Int)], Map.empty[T, Int].withDefault(_ => 0))) { case ((acc, map), e) =>
          ((e, map(e)) +: acc, map + (e -> (map(e) + 1)))
        }._1.reverse
      val (first, last) = (withRank(all.sorted), withRank(all))
      val m = seq1.size
      val mapFirstLast = (first.take(m - 1) ++ first.drop(m)).zip(last.tail).toMap
      def reconstruct(symbol: (T, Int), acc: Seq[T]): IndexedSeq[T] = mapFirstLast.get(symbol) match {
        case Some(next) => reconstruct(next, symbol._1 +: acc)
        case None => (symbol._1 +: acc).toIndexedSeq
      }
      reconstruct(last.head, Seq.empty)
    } else {
      IndexedSeq.empty
    }
  }

}
