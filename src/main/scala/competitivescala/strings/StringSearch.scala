package competitivescala.strings

object StringSearch {

  // Complexity: O(n)
  def searchStringKMP[A](string: IndexedSeq[A], word: IndexedSeq[A]): Seq[Int] =
    searchStringKMP(string, word, buildTableKMP(word))

  def searchStringKMP[A](string: IndexedSeq[A], word: IndexedSeq[A], table: IndexedSeq[Int]): Seq[Int] = {
    def scan(j: Int, k: Int, acc: Seq[Int]): Seq[Int] = {
      if (j < string.size) {
        if (word(k) == string(j)) {
          val j1 = j + 1
          val k1 = k + 1
          if (k1 == word.size) {
            scan(j1, table(k1), (j1 - k1) +: acc)
          } else {
            scan(j1, k1, acc)
          }
        } else {
          val k1 = table(k)
          if (k1 < 0) {
            scan(j + 1, k1 + 1, acc)
          } else {
            scan(j, k1, acc)
          }
        }
      } else {
        acc
      }
    }
    scan(0, 0, Seq.empty).reverse
  }

  def buildTableKMP[A](word: IndexedSeq[A]): IndexedSeq[Int] = {
    val size = word.size
    def build(pos: Int, cnd: Int, table: IndexedSeq[Int]): IndexedSeq[Int] = {
      if (pos < size) {
        if (word(pos) == word(cnd)) {
          build(pos + 1, cnd + 1, table.updated(pos, table(cnd)))
        } else {
          val updated = table.updated(pos, cnd)
          def helper(cnd: Int): Int = if (cnd >= 0 && word(pos) != word(cnd)) helper(updated(cnd)) else cnd
          build(pos + 1, helper(updated(cnd)) + 1, updated)
        }
      } else {
        table.updated(pos, cnd)
      }
    }
    build(1, 0, IndexedSeq.fill(size + 1)(0).updated(0, -1))
  }

}
