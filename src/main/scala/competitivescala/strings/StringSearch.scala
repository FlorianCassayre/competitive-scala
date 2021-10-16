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


  // Complexity: O(n)
  def searchPalindromesOddManacher[A](string: IndexedSeq[A]): IndexedSeq[Int] = {
    val n = string.size
    string.indices.foldLeft((IndexedSeq.fill(n)(0), 0, -1)) { case ((d1, l, r), i) =>
      def forward(k: Int): Int = if(k <= i && i + k < n && string(i - k) == string(i + k)) forward(k + 1) else k
      val k = if(i > r) 1 else Math.min(d1(l + r - i), r - i + 1)
      val k1 = forward(k)
      val k2 = k1 - 1
      val (l1, r1) = if(i + k2 > r) (i - k2, i + k2) else (l, r)
      (d1.updated(i, k1), l1, r1)
    }._1
  }

  // Complexity: O(n)
  def searchPalindromesEvenManacher[A](string: IndexedSeq[A]): IndexedSeq[Int] = {
    val n = string.size
    string.indices.foldLeft((IndexedSeq.fill(n)(0), 0, -1)) { case ((d2, l, r), i) =>
      def forward(k: Int): Int = if(k <= i - 1 && i + k < n && string(i - k - 1) == string(i + k)) forward(k + 1) else k
      val k = if(i > r) 0 else Math.min(d2(l + r - i + 1), r - i + 1)
      val k1 = forward(k)
      val k2 = k1 - 1
      val (l1, r1) = if(i + k2 > r) (i - k2 - 1, i + k2) else (l, r)
      (d2.updated(i, k1), l1, r1)
    }._1
  }

  def searchPalindromesPrefixManacher[A](string: IndexedSeq[A]): IndexedSeq[Int] = {
    val (d1, d2) = (searchPalindromesOddManacher(string), searchPalindromesEvenManacher(string))
    d1.zip(d2).zipWithIndex.foldRight(IndexedSeq.fill(string.size)(1)) { case (((e1, e2), i), acc) =>
      val (i1, i2) = (i - e1 + 1, i - e2)
      val acc1 = if(e1 > 1) acc.updated(i1, Math.max(2 * e1 - 1, acc(i1))) else acc
      if(e2 > 0) acc1.updated(i2, Math.max(2 * e2, acc1(i2))) else acc1
    }.scanLeft(1)((acc, e) => Math.max(acc - 2, e)).tail
  }

}
