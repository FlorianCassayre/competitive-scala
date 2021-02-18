package competitivescala.numbers

import java.math.MathContext

object RationalArithmetic {

  sealed case class Rational[N] private (a: N, b: N)(implicit ev: Integral[N]) {
    import ev._

    override def toString: String = s"$a/$b"

    def toContinuedFraction: Seq[N] = {
      def expand(x: Rational[N], acc: Seq[N]): Seq[N] = {
        import scala.math.Fractional.Implicits.infixFractionalOps
        if(x.b != one) {
          val a = x.a / x.b // Floor division
          expand(Rational.RationalFractional.one / (x - Rational(a)), a +: acc)
        } else {
          x.a +: acc
        }
      }
      expand(this, Seq.empty).reverse
    }
  }

  object Rational {
    def apply[N](a: N, b: N)(implicit ev: Integral[N]): Rational[N] = {
      import ev._
      require(b != zero)
      def gcd(a: N, b: N): N = if (b == zero) a.abs else gcd(b, a % b)
      val n = gcd(a, b)
      new Rational(sign(a * b) * a.abs / n, b.abs / n)
    }

    def apply[N](a: N)(implicit ev: Integral[N]): Rational[N] = apply(a, ev.one)

    // c(0) + 1 / (c(1) + 1 / (c(2) + 1 / ...))
    def fromContinuedFraction[N](coefficients: Seq[N])(implicit ev: Integral[N]): Rational[N] = {
      import scala.math.Fractional.Implicits.infixFractionalOps
      val one = Rational(ev.one)
      coefficients.map(apply(_)).reduceRight((a, b) => a + one / b)
    }

    class RationalIntegral[N](implicit ev: Integral[N]) extends Fractional[Rational[N]] {
      import scala.math.Integral.Implicits._
      override def plus(x: Rational[N], y: Rational[N]): Rational[N] = Rational(x.a * y.b + x.b * y.a, x.b * y.b)
      override def minus(x: Rational[N], y: Rational[N]): Rational[N] = Rational(x.a * y.b - x.b * y.a, x.b * y.b)
      override def times(x: Rational[N], y: Rational[N]): Rational[N] = Rational(x.a * y.a, x.b * y.b)
      override def div(x: Rational[N], y: Rational[N]): Rational[N] = Rational(x.a * y.b, x.b * y.a)
      override def negate(x: Rational[N]): Rational[N] = Rational(-x.a, x.b)
      override def fromInt(x: Int): Rational[N] = Rational(ev.fromInt(x), ev.one)
      override def parseString(str: String): Option[Rational[N]] = str match {
        case s"$a/$b" =>
          (ev.parseString(a), ev.parseString(b)) match {
            case (Some(na), Some(nb)) => Some(Rational(na, nb))
            case _ => None
          }
        case _ => None
      }
      override def toInt(x: Rational[N]): Int = ev.quot(x.a, x.b).toInt
      override def toLong(x: Rational[N]): Long = ev.quot(x.a, x.b).toLong
      override def toFloat(x: Rational[N]): Float = x.a.toFloat / x.b.toFloat
      override def toDouble(x: Rational[N]): Double = x.a.toDouble / x.b.toDouble
      override def compare(x: Rational[N], y: Rational[N]): Int = ev.compare(x.a * y.b, x.b * y.a)

      def isIntegral(x: Rational[N]): Boolean = x.b == ev.one
    }

    implicit def RationalFractional[N](implicit ev: Integral[N]): Fractional[Rational[N]] = new RationalIntegral[N]
  }

  type BigRational = Rational[BigInt]
  def BigRational(numerator: BigInt, denominator: BigInt = 1): BigRational = Rational(numerator, denominator)
  val BigRationalFractional: Fractional[Rational[BigInt]] = Rational.RationalFractional[BigInt]

  implicit class BigRationalWrapper(br: BigRational) {
    def toBigDecimal(mc: MathContext): BigDecimal = BigDecimal(br.a, mc) / BigDecimal(br.b, mc)
  }

  import scala.math.Fractional.Implicits.infixFractionalOps // Mandatory upon usage

}
