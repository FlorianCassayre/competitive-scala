package competitivescala.strings

import org.scalatest.funsuite.AnyFunSuite
import StringSearch._

import scala.util.Random

class StringSearchTest extends AnyFunSuite {

  test("kmp build table") {
    assert(buildTableKMP("ABCDABD") == IndexedSeq(-1, 0, 0, 0, -1, 0, 2, 0))
    assert(buildTableKMP("ABACABABC") == IndexedSeq(-1, 0, -1, 1, -1, 0, -1, 3, 2, 0))
    assert(buildTableKMP("ABACABABA") == IndexedSeq(-1, 0, -1, 1, -1, 0, -1, 3, -1, 3))
    assert(buildTableKMP("PARTICIPATE IN PARACHUTE") == IndexedSeq(
      -1, 0, 0, 0, 0, 0, 0, -1, 0, 2, 0, 0, 0, 0, 0, -1, 0, 0, 3, 0, 0, 0, 0, 0, 0
    ))
  }

  test("kmp search string") {
    assert(searchStringKMP("ABC ABCDAB ABCDABCDABDE", "ABCDABD") == Seq(15))
    assert(searchStringKMP("ABABABCABA", "ABA") == Seq(0, 2, 7))
    assert(searchStringKMP("AAAA", "AA") == Seq(0, 1, 2))
    assert(searchStringKMP("ABC", "ABC") == Seq(0))
    assert(searchStringKMP("ABC", "D") == Seq())
    assert(searchStringKMP("ABC", "ABCD") == Seq())
    assert(searchStringKMP("ABC", "DEFG") == Seq())
  }

  test("palindrome even and odd search") {
    assert(searchPalindromesOddManacher("") == IndexedSeq.empty)
    assert(searchPalindromesEvenManacher("") == IndexedSeq.empty)
    assert(searchPalindromesOddManacher("aa") == IndexedSeq(1, 1))
    assert(searchPalindromesEvenManacher("aa") == IndexedSeq(0, 1))
    assert(searchPalindromesOddManacher("ababc") == IndexedSeq(1, 2, 2, 1, 1))
    assert(searchPalindromesEvenManacher("ababc") == IndexedSeq(0, 0, 0, 0, 0))
    assert(searchPalindromesOddManacher("ababbac") == IndexedSeq(1, 2, 2, 1, 1, 1, 1))
    assert(searchPalindromesEvenManacher("ababbac") == IndexedSeq(0, 0, 0, 0, 2, 0, 0))
  }

  test("palindrome general prefix search") {
    assert(searchPalindromesPrefixManacher("") == IndexedSeq.empty)
    assert(searchPalindromesPrefixManacher("a") == IndexedSeq(1))
    assert(searchPalindromesPrefixManacher("ab") == IndexedSeq(1, 1))
    assert(searchPalindromesPrefixManacher("aa") == IndexedSeq(2, 1))
    assert(searchPalindromesPrefixManacher("abbcbaba") == IndexedSeq(1, 2, 3, 1, 3, 3, 1, 1))
    assert(searchPalindromesPrefixManacher("aaabbbba") == IndexedSeq(3, 2, 6, 4, 3, 2, 1, 1))
    assert(searchPalindromesPrefixManacher("abcdcba") == IndexedSeq(7, 5, 3, 1, 1, 1, 1))
  }

  test("palindrome general prefix search random") {
    def safeImpl[T](s: IndexedSeq[T]): IndexedSeq[Int] = {
      val n = s.size
      def isPalindrome(seq: Seq[T]): Boolean = seq == seq.reverse
      (0 until n).map(i => (i until n).filter(j => isPalindrome(s.slice(i, j + 1))).max - i + 1)
    }
    val rnd = new Random(42)
    for(_ <- 0 until 10) {
      val n = 250 + rnd.nextInt(100)
      val k = 3 + rnd.nextInt(5)
      val s = IndexedSeq.fill(n)(('a' + rnd.nextInt(k)).toChar).mkString
      assert(searchPalindromesPrefixManacher(s) == safeImpl(s))
    }
  }

}
