package competitivescala.strings

import SuffixArray._

import org.scalatest.funsuite.AnyFunSuite

class SuffixArrayTest extends AnyFunSuite {

  test("suffix array") {
    assert(sortSuffixes("") == IndexedSeq.empty)
    assert(sortSuffixes("a") == IndexedSeq(0))
    assert(sortSuffixes("ba") == IndexedSeq(1, 0))
    assert(sortSuffixes("aaa") == IndexedSeq(2, 1, 0))
    assert(sortSuffixes("banana") == IndexedSeq(5, 3, 1, 0, 4, 2))
    assert(sortSuffixes("baobab") == IndexedSeq(4, 1, 5, 3, 0, 2))
  }

  test("cyclic array") {
    assert(sortRotations("") == IndexedSeq.empty)
    assert(sortRotations("a") == IndexedSeq(0))
    assert(sortRotations("ba") == IndexedSeq(1, 0))
    assert(sortRotations("aaa") == IndexedSeq(0, 1, 2))
    assert(sortRotations("banana") == IndexedSeq(5, 3, 1, 0, 4, 2))
    assert(sortRotations("baobab") == IndexedSeq(4, 1, 3, 0, 5, 2))
  }

  test("burrows-wheeler roundtrip") {
    def testRoundtrip(string: String): Unit = {
      val (index, seq) = burrowsWheelerTransform(string)
      val result = burrowsWheelerInverseTransform(index, seq)
      assert(result.mkString == string)
    }

    testRoundtrip("")
    testRoundtrip("a")
    testRoundtrip("ab")
    testRoundtrip("aa")
    testRoundtrip("banana")
    testRoundtrip("mississippi")
    testRoundtrip("panamabananas")

    assert(burrowsWheelerTransform("java") == (2, "vjaa".toIndexedSeq))
    assert(burrowsWheelerTransform("panamabananas") == (11, "mnpbnnaaaaasa".toIndexedSeq))
  }

  test("kasai longest common prefix array") {
    assert(longestCommonPrefixArray("") == IndexedSeq.empty)
    assert(longestCommonPrefixArray("a") == IndexedSeq(0))
    assert(longestCommonPrefixArray("banana") == IndexedSeq(1, 3, 0, 0, 2, 0))
    assert(longestCommonPrefixArray("mississippi") == IndexedSeq(1, 1, 4, 0, 0, 1, 0, 2, 1, 3, 0))
  }

}
