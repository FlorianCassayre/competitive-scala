package algorithms.string

import org.scalatest.funsuite.AnyFunSuite

import StringSearch._

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

}
