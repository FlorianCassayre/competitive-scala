package competitivescala.utils

import org.scalatest.funsuite.AnyFunSuite
import SpaceFillingCurves._

import scala.collection.View
import scala.util.Random

class SpaceFillingCurvesTest extends AnyFunSuite {

  type From = Int => Int => (Int, Int)
  type To = Int => (Int, Int) => Int
  type Check = Int => (Int, (Int, Int)) => Unit

  val checkGeneric: (From, To) => Check = (from, to) => n => (i, p) => {
    val (x, y) = p
    assert(from(n)(i) == p)
    assert(to(n)(x, y) == i)
  }

  def checkRandomIdentity(from: From, to: To): Unit = {
    val random = new Random(1)
    View.fill(1000)(random.nextInt(10)).foreach { n =>
      val i = random.nextInt((1 << 2) << (n << 1))
      val (x, y) = from(n)(i)
      val j = to(n)(x, y)
      assert(i == j)
    }
  }

  test("binary indexing curve") {
    val check = checkGeneric(binaryFromIndex, binaryToIndex)

    check(0)(0, (0, 0))
    check(0)(1, (0, 1))
    check(0)(2, (1, 0))
    check(0)(3, (1, 1))

    check(1)(3, (1, 1))
    check(1)(5, (0, 3))
    check(1)(7, (1, 3))
    check(1)(10, (3, 0))
    check(1)(13, (2, 3))

    check(2)(43, (7, 1))

    checkRandomIdentity(binaryFromIndex, binaryToIndex)
  }

  test("gray code indexing curve") {
    val check = checkGeneric(grayCodeFromIndex, grayCodeToIndex)

    check(0)(0, (0, 0))
    check(0)(1, (0, 1))
    check(0)(2, (1, 1))
    check(0)(3, (1, 0))

    check(1)(7, (0, 2))

    checkRandomIdentity(grayCodeFromIndex, grayCodeToIndex)
  }

  test("hilbert curve") {
    val check = checkGeneric(hilbertFromIndex, hilbertToIndex)

    check(0)(0, (0, 0))
    check(0)(1, (0, 1))
    check(0)(2, (1, 1))
    check(0)(3, (1, 0))

    check(1)(1, (1, 0))
    check(1)(2, (1, 1))
    check(1)(3, (0, 1))
    check(1)(6, (1, 3))
    check(1)(8, (2, 2))
    check(1)(14, (2, 0))

    check(2)(19, (0, 5))

    checkRandomIdentity(hilbertFromIndex, hilbertToIndex)
  }

}
