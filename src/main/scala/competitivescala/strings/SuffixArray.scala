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

}
