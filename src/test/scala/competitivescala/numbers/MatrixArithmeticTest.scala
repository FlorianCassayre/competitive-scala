package competitivescala.numbers

import competitivescala.numbers.MatrixArithmetic._
import org.scalactic.Equality
import org.scalactic.Tolerance._
import org.scalatest.funsuite.AnyFunSuite


class MatrixArithmeticTest extends AnyFunSuite {

  private implicit val matrixEq: Equality[IndexedSeq[IndexedSeq[Double]]] =
    (a, ba) => ba match {
      case b: IndexedSeq[IndexedSeq[Double]] =>
        a.size == b.size &&
          a.zip(b).forall { case (a1, b1) => a1.size == b1.size } &&
          a.flatten.zip(b.flatten).forall { case (a1, b1) => a1 === b1 +- 1E-3 }
      case _ => false
    }
  private val precision = 1E-6

  test("matrix multiplication") {
    assert(multiply(IndexedSeq(
      IndexedSeq(1, 2, 3),
      IndexedSeq(4, 5, 6)
    ), IndexedSeq(
      IndexedSeq(10, 11),
      IndexedSeq(20, 21),
      IndexedSeq(30, 31)
    )) == IndexedSeq(
      IndexedSeq(140, 146),
      IndexedSeq(320, 335)
    ))
  }

  test("gaussian elimination") {
    assert(gaussianElimination(IndexedSeq(IndexedSeq(2.5)), precision) === IndexedSeq(IndexedSeq(1.0)))
    assert(gaussianElimination(IndexedSeq(IndexedSeq(1.5, 2.0), IndexedSeq(-1.0, 0.5)), precision) === IndexedSeq(IndexedSeq(1.0, 0.0), IndexedSeq(0.0, 1.0)))
    assert(gaussianElimination(IndexedSeq(
      IndexedSeq(2.0, -1.0, 0.0, 1.0),
      IndexedSeq(-1.0, 2.0, -1.0, 2.0),
      IndexedSeq(0.0, -1.0, -2.0, 3.0)
    ), precision) === IndexedSeq(
      IndexedSeq(1.0, 0.0, 0.0, 0.75),
      IndexedSeq(0.0, 1.0, 0.0, 0.5),
      IndexedSeq(0.0, 0.0, 1.0, -1.75)
    ))
  }

  test("matrix inverse") {
    assert(inverse(IndexedSeq(
      IndexedSeq(2.0, -1.0, 0.0),
      IndexedSeq(-1.0, 2.0, -1.0),
      IndexedSeq(0.0, -1.0, 2.0)
    ), precision).get === IndexedSeq(
      IndexedSeq(0.75, 0.5, 0.25),
      IndexedSeq(0.5, 1.0, 0.5),
      IndexedSeq(0.25, 0.5, 0.75)
    ))
  }

  test("gaussian elimination group") {
    assert(gaussianEliminationGroup(IndexedSeq(
      IndexedSeq(1, 1, 0, 1),
      IndexedSeq(1, 0, 1, 1),
      IndexedSeq(0, 1, 0, 0)
    ), 2) == IndexedSeq(
      IndexedSeq(1, 0, 0, 1),
      IndexedSeq(0, 1, 0, 0),
      IndexedSeq(0, 0, 1, 0)
    ))
    assert(gaussianEliminationGroup(IndexedSeq(
      IndexedSeq(2, -3, 0, 1),
      IndexedSeq(-1, 1, -9, 3),
      IndexedSeq(10, 2, -14, 0)
    ), 7) == IndexedSeq(
      IndexedSeq(1, 0, 0, 5),
      IndexedSeq(0, 1, 0, 3),
      IndexedSeq(0, 0, 1, 1)
    ))
  }

  test("matrix inverse group") {
    assert(inverseGroup(IndexedSeq(
      IndexedSeq(2, 8, 0),
      IndexedSeq(1, 9, 2),
      IndexedSeq(5, 1, 9)
    ), 11).get == IndexedSeq(
      IndexedSeq(2, 5, 5),
      IndexedSeq(1, 7, 7),
      IndexedSeq(0, 5, 10)
    ))
  }

  test("clockwise rotation") {
    val matrix = IndexedSeq(
      IndexedSeq(1, 2, 3),
      IndexedSeq(4, 5, 6)
    )
    assert(rotateClockwise(matrix, 0) == matrix)
    assert(rotateClockwise(matrix) == IndexedSeq(
      IndexedSeq(4, 1),
      IndexedSeq(5, 2),
      IndexedSeq(6, 3)
    ))
    assert(rotateClockwise(matrix, 2) == IndexedSeq(
      IndexedSeq(6, 5, 4),
      IndexedSeq(3, 2, 1)
    ))
    assert(rotateClockwise(matrix, 3) == IndexedSeq(
      IndexedSeq(3, 6),
      IndexedSeq(2, 5),
      IndexedSeq(1, 4)
    ))
  }

}
