package competitivescala.utils

import BinarySearch._

import org.scalatest.funsuite.AnyFunSuite

class BinarySearchTest extends AnyFunSuite {

  test("binary search") {
    assert(binarySearchArray(IndexedSeq(), 1).isEmpty)
    assert(binarySearchArray(IndexedSeq(1, 2, 3), 0).isEmpty)
    assert(binarySearchArray(IndexedSeq(1, 2, 3, 4), 0).isEmpty)
    assert(binarySearchArray(IndexedSeq(1, 2, 3), 4).isEmpty)
    assert(binarySearchArray(IndexedSeq(1, 2, 3, 4), 5).isEmpty)
    assert(binarySearchArray(IndexedSeq(1), 1).contains(0))
    assert(binarySearchArray(IndexedSeq(1, 2, 3), 1).contains(0))
    assert(binarySearchArray(IndexedSeq(1, 2, 3), 2).contains(1))
    assert(binarySearchArray(IndexedSeq(1, 2, 3), 3).contains(2))
    assert(binarySearchArray(IndexedSeq(1, 2, 3, 4), 1).contains(0))
    assert(binarySearchArray(IndexedSeq(1, 2, 3, 4), 2).contains(1))
    assert(binarySearchArray(IndexedSeq(1, 2, 3, 4), 3).contains(2))
    assert(binarySearchArray(IndexedSeq(1, 2, 3, 4), 4).contains(3))
    assert(binarySearchArray(IndexedSeq(1, 2, 2, 3), 2).nonEmpty)
  }

  test("bounded binary search") {
    assert(binarySearchBoundArray(IndexedSeq(), 0, upper = false).isEmpty)
    assert(binarySearchBoundArray(IndexedSeq(1, 2, 3), 2, upper = false).contains(1))
    assert(binarySearchBoundArray(IndexedSeq(1, 2, 3, 4), 2, upper = false).contains(1))
    assert(binarySearchBoundArray(IndexedSeq(1, 2, 2, 3), 2, upper = false).contains(1))
    assert(binarySearchBoundArray(IndexedSeq(1, 2, 2, 3), 2, upper = false).contains(1))
    assert(binarySearchBoundArray(IndexedSeq(1, 2, 2, 3), 2, upper = true).contains(2))
    assert(binarySearchBoundArray(IndexedSeq(1, 2, 2, 2, 3), 2, upper = false).contains(1))
    assert(binarySearchBoundArray(IndexedSeq(1, 2, 2, 2, 3), 2, upper = true).contains(3))
    assert(binarySearchBoundArray(IndexedSeq(1, 2, 3, 3, 4, 5), 3, upper = false, strict = true).contains(4))
    assert(binarySearchBoundArray(IndexedSeq(1, 2, 3, 3, 4, 5), 3, upper = true, strict = true).contains(1))
    assert(binarySearchBoundArray(IndexedSeq(1, 1), 2, upper = false, strict = true).isEmpty)
    assert(binarySearchBoundArray(IndexedSeq(1, 1, 1), 2, upper = false, strict = true).isEmpty)
    assert(binarySearchBoundArray(IndexedSeq(1, 1, 1), 0, upper = true, strict = true).isEmpty)
  }

}
