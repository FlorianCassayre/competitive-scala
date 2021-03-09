package competitivescala.strings

object SuffixArray {

  // Complexity: O(n log n)
  // Requires a injection between T and Ints
  def sortSuffixes[T](seq: IndexedSeq[T])(implicit toInt: T => Int): IndexedSeq[Int] = {
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
    iteratePow(1, seq.indices.reverse.sortBy(seq.andThen(toInt)), seq.map(toInt))
  }

  // Complexity: O(n log n)
  def burrowsWheelerTransform[T](seq: IndexedSeq[T])(implicit toInt: T => Int): (Int, IndexedSeq[T]) = {
    val sorted = sortSuffixes(seq)
    val n = seq.size
    (sorted.indexOf(0), sorted.map(i => seq((i + n - 1) % n)))
  }

  // Complexity: O(n log n)
  def burrowsWheelerInverseTransform[T](index: Int, seq: IndexedSeq[T])(implicit toInt: T => Int): IndexedSeq[T] = {
    if(seq.nonEmpty) {
      def withRank(seq: Seq[T]): Seq[(T, Int)] =
        seq.foldLeft((Seq.empty[(T, Int)], Map.empty[T, Int].withDefault(_ => 0))) { case ((acc, map), e) =>
          ((e, map(e)) +: acc, map + (e -> (map(e) + 1)))
        }._1.reverse
      val last = withRank(seq)
      val mapFirstLast = withRank(seq.sortBy(toInt)).zip(last).toMap
      seq.foldLeft((last(index), Seq.empty[T])) { case ((symbol, acc), _) => (mapFirstLast(symbol), symbol._1 +: acc) }._2.toIndexedSeq
    } else {
      IndexedSeq.empty
    }
  }


  // Complexity: O(n log n)
  def sortRotations[T](seq: IndexedSeq[T])(implicit toInt: T => Int): IndexedSeq[Int] = {
    val n = seq.size
    val indices = seq.indices
    def iteratePow(k: Int, array: IndexedSeq[Int], ranks: IndexedSeq[Int]): IndexedSeq[Int] = {
      if(k < n) {
        val hk = k / 2
        val newRanks = indices.foldLeft(ranks)((currentRanks, i) =>
          currentRanks.updated(array(i),
            if(i > 0 &&
              ranks(array(i - 1)) == ranks(array(i)) &&
              ranks((array(i - 1) + hk) % n) == ranks((array(i) + hk) % n))
              currentRanks(array(i - 1))
            else
              i
          )
        )
        val (newArray, _) = indices.foldLeft((array, indices.toVector)) { case ((currentArray, currentCount), i) =>
          val s = (array(i) - k + n) % n
          val r = newRanks(s)
          (currentArray.updated(currentCount(r), s), currentCount.updated(r, currentCount(r) + 1))
        }
        iteratePow(k * 2, newArray, newRanks)
      } else {
        array
      }
    }
    iteratePow(1, seq.indices.sortBy(seq.andThen(toInt)), seq.map(toInt))
  }

}
