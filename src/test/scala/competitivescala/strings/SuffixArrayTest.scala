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
  }

}
