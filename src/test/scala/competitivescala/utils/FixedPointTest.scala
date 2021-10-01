package competitivescala.utils

import org.scalatest.funsuite.AnyFunSuite

import competitivescala.utils.FixedPoint._

class FixedPointTest extends AnyFunSuite {

  test("fixed point identity function") {
    def test(n: Long): Unit = assert(applyRecursiveMemoized(identity[String])("x", n) == "x")
    test(0)
    test(1)
    test(2)
    test(1_000_000_000_000_000L)
  }

  test("orbit simple cycle") {
    val mapping = IndexedSeq(2, 3, 1, 0)
    assert(applyRecursiveMemoized(mapping)(0, 0) == 0)
    assert(applyRecursiveMemoized(mapping)(2, 0) == 2)
    assert(applyRecursiveMemoized(mapping)(0, 1) == 2)
    assert(applyRecursiveMemoized(mapping)(0, 2) == 1)
    assert(applyRecursiveMemoized(mapping)(0, 3) == 3)
    assert(applyRecursiveMemoized(mapping)(0, 4) == 0)
    assert(applyRecursiveMemoized(mapping)(0, 5) == 2)
    assert(applyRecursiveMemoized(mapping)(0, 9) == 2)
    assert(applyRecursiveMemoized(mapping)(0, 10) == 1)
  }

  test("orbit no cycle") {
    def next(n: Int): Int = n + 1
    assert(applyRecursiveMemoized(next)(0, 0) == 0)
    assert(applyRecursiveMemoized(next)(0, 1) == 1)
    assert(applyRecursiveMemoized(next)(0, 2) == 2)
    assert(applyRecursiveMemoized(next)(0, 3) == 3)
    assert(applyRecursiveMemoized(next)(5, 10) == 15)
    // No optimization
  }

  test("orbit chain with cycle") {
    val mapping = IndexedSeq(1, 2, 3, 4, 2)
    assert(applyRecursiveMemoized(mapping)(0, 1) == 1)
    assert(applyRecursiveMemoized(mapping)(0, 2) == 2)
    assert(applyRecursiveMemoized(mapping)(0, 3) == 3)
    assert(applyRecursiveMemoized(mapping)(0, 4) == 4)
    assert(applyRecursiveMemoized(mapping)(0, 5) == 2)
    assert(applyRecursiveMemoized(mapping)(0, 6) == 3)
    assert(applyRecursiveMemoized(mapping)(0, 8) == 2)
    assert(applyRecursiveMemoized(mapping)(0, 1_000_000_000_000_000L) == 4)
    assert(applyRecursiveMemoized(mapping)(1, 1_000_000_000_000_000L) == 2)
    assert(applyRecursiveMemoized(mapping)(3, 1_000_000_000_000_000L) == 4)
  }

}
