package competitivescala.numbers

import org.scalatest.funsuite.AnyFunSuite

import RationalArithmetic._

class RationalArithmeticTest extends AnyFunSuite {

  import scala.math.Fractional.Implicits.infixFractionalOps

  test("test big rationals") {
    assert(BigRationalFractional.zero == BigRational(0))
    assert(BigRationalFractional.one == BigRational(1))

    assert(BigRational(1) != BigRational(2))
    assert(BigRational(1, 2) == BigRational(2, 4))
    assert(BigRational(0, 2) == BigRational(0, -3))
    assert(BigRational(-12, 28) == BigRational(9, -21))

    assert(BigRational(2, 5) + BigRational(2, 3) == BigRational(16, 15))
    assert(BigRational(2, 5) - BigRational(2, 3) == BigRational(-4, 15))
    assert(BigRational(2, 5) * BigRational(-2, 3) == BigRational(-4, 15))
    assert(BigRational(2, 5) / BigRational(-2, 3) == BigRational(-3, 5))

    assert(BigRational(-3, -87) > BigRationalFractional.zero)
    assert(BigRational(2, -21) < BigRationalFractional.zero)

    assertThrows[Exception](BigRational(1, 0))
  }

}
