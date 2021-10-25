package competitivescala.numbers

import org.scalatest.funsuite.AnyFunSuite
import FFT._

import scala.util.Random

class FFTTest extends AnyFunSuite {

  private val error = 1e-6

  implicit class ComplexWrapper(c1: Complex) {
    def =~=(c2: Complex): Boolean = Math.abs(c1.r - c2.r) <= error && Math.abs(c1.i - c2.i) <= error
  }
  implicit class ComplexSeqWrapper(s1: IndexedSeq[Complex]) {
    def =~=(s2: IndexedSeq[Complex]): Boolean = s1.size == s2.size && s1.zip(s2).forall { case (a, b) => a =~= b }
  }

  test("fft small") {
    assert(fft(IndexedSeq.empty) == IndexedSeq.empty)
    assert(fft(Complex.from(IndexedSeq(2))) == IndexedSeq(Complex(2)))

    val array = IndexedSeq(1, 2, 3, 4)
    val transformed = fft(Complex.from(array))
    assert(transformed =~= IndexedSeq(Complex(10), Complex(-2, -2), Complex(-2), Complex(-2, 2)))
  }

  test("fft non radix 2") {
    val array1 = IndexedSeq(1, 2, 3)
    assert(fft(Complex.from(array1)) =~= IndexedSeq(Complex(6), Complex(-2, 2), Complex(2), Complex(-2, -2)))

    assert(fft(Complex.from(IndexedSeq.fill(5)(0))).size == 8)
    assert(fft(Complex.from(IndexedSeq.fill(7)(0))).size == 8)
    assert(fft(Complex.from(IndexedSeq.fill(9)(0))).size == 16)
    assert(fft(Complex.from(IndexedSeq.fill(12)(0))).size == 16)
  }

  test("fft roundtrip") {
    val random = new Random(1)
    for(_ <- 0 until 100) {
      val n = 50 + random.nextInt(50)
      val array = IndexedSeq.fill(n)(random.nextInt(51) - 25)
      val complex = Complex.from(array)
      val (padded, log) = pad(complex, Complex(0))
      val transformed = fft(padded, log)
      assert(ifft(transformed, log) =~= padded)
    }
  }

  test("fft convolution") {
    def convInt(array: IndexedSeq[Int], pattern: IndexedSeq[Int], valid: Boolean = true): IndexedSeq[Int] =
      convolution1D(array, pattern, valid).map(Math.round).map(_.toInt)

    assert(convInt(IndexedSeq(2), IndexedSeq(3)) == IndexedSeq(6))
    assert(convInt(IndexedSeq(4, 5, 6), IndexedSeq(2, 3)) == IndexedSeq(23, 28))
    assert(convInt(IndexedSeq(1, 2, 3), IndexedSeq(2)) == IndexedSeq(2, 4, 6))
    assert(convInt(IndexedSeq(1, 2, 3, 4, 5), IndexedSeq(2, 3, 4)) == IndexedSeq(20, 29, 38))
    assert(convInt(IndexedSeq(2, 3), IndexedSeq(4, 5, 6, 7), valid = false) == IndexedSeq(14, 33, 28, 23, 12))
    assert(convInt(IndexedSeq(1, 2, 3), IndexedSeq(5, 6, 7), valid = false) == IndexedSeq(7, 20, 38, 28, 15))
  }
}
