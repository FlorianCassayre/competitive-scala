package competitivescala.numbers

import Combinatorics._

import org.scalatest.funsuite.AnyFunSuite

class CombinatoricsTest extends AnyFunSuite {

  test("factorial") {
    assert(factorial(0) == 1)
    assert(factorial(1) == 1)
    assert(factorial(2) == 2)
    assert(factorial(3) == 6)
    assert(factorial(10) == 3628800)
    assert(factorial(BigInt(42)).toString == "1405006117752879898543142606244511569936384000000000")
  }

  test("partial factorial") {
    assert(partialFactorial(6, 6) == 6)
    assert(partialFactorial(0, 10) == 3628800)
    assert(partialFactorial(5, 8) == 1680)
  }

  test("combinations") {
    assert(combinations(0, 0) == 1)
    assert(combinations(1, 0) == 1)
    assert(combinations(0, 1) == 0)
    assert(combinations(1, 1) == 1)
    assert(combinations(-1, 1) == 0)
    assert(combinations(1, -1) == 0)
    assert(combinations(-1, -2) == 0)
    assert(combinations(1, 2) == 0)
    assert(combinations(2, 0) == 1)
    assert(combinations(2, 1) == 2)
    assert(combinations(2, 2) == 1)
    assert(combinations(5, 2) == 10)
    assert(combinations(5, 2) == 10)
    assert(combinations(10, 7) == 120)
    assert(combinations(13, 5) == 1287)
  }

  test("permutations") {
    def stringPermutations(s: String): Int = permutations[Char, Int](s)
    assert(stringPermutations("") == 1)
    assert(stringPermutations("a") == 1)
    assert(stringPermutations("ab") == 2)
    assert(stringPermutations("aaa") == 1)
    assert(stringPermutations("aab") == 3)
    assert(stringPermutations("aabb") == 6)
    assert(stringPermutations("abc") == 6)
    assert(stringPermutations("abaacba") == 105)
  }

}
