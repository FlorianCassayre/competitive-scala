package competitivescala.utils

import BinarySearch._

import org.scalatest.funsuite.AnyFunSuite

class BinarySearchTest extends AnyFunSuite {

  test("binary search") {
    assert(binarySearch(IndexedSeq(), 1).isEmpty)
    assert(binarySearch(IndexedSeq(1, 2, 3), 0).isEmpty)
    assert(binarySearch(IndexedSeq(1, 2, 3, 4), 0).isEmpty)
    assert(binarySearch(IndexedSeq(1, 2, 3), 4).isEmpty)
    assert(binarySearch(IndexedSeq(1, 2, 3, 4), 5).isEmpty)
    assert(binarySearch(IndexedSeq(1), 1).contains(0))
    assert(binarySearch(IndexedSeq(1, 2, 3), 1).contains(0))
    assert(binarySearch(IndexedSeq(1, 2, 3), 2).contains(1))
    assert(binarySearch(IndexedSeq(1, 2, 3), 3).contains(2))
    assert(binarySearch(IndexedSeq(1, 2, 3, 4), 1).contains(0))
    assert(binarySearch(IndexedSeq(1, 2, 3, 4), 2).contains(1))
    assert(binarySearch(IndexedSeq(1, 2, 3, 4), 3).contains(2))
    assert(binarySearch(IndexedSeq(1, 2, 3, 4), 4).contains(3))
    assert(binarySearch(IndexedSeq(1, 2, 2, 3), 2).nonEmpty)
  }

  test("bounded binary search") {
    assert(binarySearchBound(IndexedSeq(), 0, upper = false).isEmpty)
    assert(binarySearchBound(IndexedSeq(1, 2, 3), 2, upper = false).contains(1))
    assert(binarySearchBound(IndexedSeq(1, 2, 3, 4), 2, upper = false).contains(1))
    assert(binarySearchBound(IndexedSeq(1, 2, 2, 3), 2, upper = false).contains(1))
    assert(binarySearchBound(IndexedSeq(1, 2, 2, 3), 2, upper = false).contains(1))
    assert(binarySearchBound(IndexedSeq(1, 2, 2, 3), 2, upper = true).contains(2))
    assert(binarySearchBound(IndexedSeq(1, 2, 2, 2, 3), 2, upper = false).contains(1))
    assert(binarySearchBound(IndexedSeq(1, 2, 2, 2, 3), 2, upper = true).contains(3))
    assert(binarySearchBound(IndexedSeq(1, 2, 3, 3, 4, 5), 3, upper = false, strict = true).contains(4))
    assert(binarySearchBound(IndexedSeq(1, 2, 3, 3, 4, 5), 3, upper = true, strict = true).contains(1))
    assert(binarySearchBound(IndexedSeq(1, 1), 2, upper = false, strict = true).isEmpty)
    assert(binarySearchBound(IndexedSeq(1, 1, 1), 2, upper = false, strict = true).isEmpty)
    assert(binarySearchBound(IndexedSeq(1, 1, 1), 0, upper = true, strict = true).isEmpty)
  }

}
