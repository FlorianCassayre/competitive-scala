package competitivescala.utils

import RomanNumerals._

import org.scalatest.funsuite.AnyFunSuite

class RomanNumeralsTest extends AnyFunSuite {

  test("roman numerals") {
    assert(toRomanNumerals(1) == "I")
    assert(toRomanNumerals(2) == "II")
    assert(toRomanNumerals(4) == "IV")
    assert(toRomanNumerals(4) == "IV")
    assert(toRomanNumerals(42) == "XLII")
    assert(toRomanNumerals(999) == "CMXCIX")
    assert(toRomanNumerals(2021) == "MMXXI")

    (1 until 4000).foreach(n => assert(parseRomanNumerals[Int](toRomanNumerals(n)) == n))
  }

}
