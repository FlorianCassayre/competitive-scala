package competitivescala.strings

import RunLength._

import org.scalatest.funsuite.AnyFunSuite

class RunLengthTest extends AnyFunSuite {

  test("run length encode") {
    assert(runLengthEncode("") == Seq())
    assert(runLengthEncode("a") == Seq(('a', 1)))
    assert(runLengthEncode("abbbcddeee") == Seq(('a', 1), ('b', 3), ('c', 1), ('d', 2), ('e', 3)))
  }

  test("run length decode") {
    assert(runLengthDecode(Seq()).mkString == "")
    assert(runLengthDecode(Seq(('a', 2))).mkString == "aa")
    assert(runLengthDecode(Seq(('a', 3), ('b', 1), ('c', 2))).mkString == "aaabcc")
  }

  test("run length roundtrip") {
    def testRoundtrip(s: String): Unit = {
      assert(runLengthDecode(runLengthEncode(s)).mkString == s)
    }
    testRoundtrip("edddcbbba")
    testRoundtrip("loop da loop")
    testRoundtrip("test")
  }

}
