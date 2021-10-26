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
  implicit class ComplexSeq2Wrapper(s1: IndexedSeq[IndexedSeq[Complex]]) {
    def =~=(s2: IndexedSeq[IndexedSeq[Complex]]): Boolean = s1.size == s2.size && s1.zip(s2).forall { case (sa, sb) => sa =~= sb }
  }

  test("fft 1D small") {
    assert(fft1D(IndexedSeq.empty, -1) == IndexedSeq.empty)
    assert(fft1D(Complex.from(IndexedSeq(2)), 0) == IndexedSeq(Complex(2)))

    val array = IndexedSeq(1, 2, 3, 4)
    val transformed = fft1D(Complex.from(array), 2)
    assert(transformed =~= IndexedSeq(Complex(10), Complex(-2, 2), Complex(-2), Complex(-2, -2)))
  }

  test("fft 1D radix 2 padding") {
    assert(padTwo1D(Complex.from(IndexedSeq.fill(5)(0)))._1.size == 8)
    assert(padTwo1D(Complex.from(IndexedSeq.fill(7)(0)))._1.size == 8)
    assert(padTwo1D(Complex.from(IndexedSeq.fill(9)(0)))._1.size == 16)
    assert(padTwo1D(Complex.from(IndexedSeq.fill(12)(0)))._1.size == 16)
  }

  test("fft 1D roundtrip") {
    val random = new Random(1)
    for(_ <- 0 until 100) {
      val n = random.nextInt(100)
      val array = IndexedSeq.fill(n)(random.nextInt(50 + 1) - 25)
      val complex = Complex.from(array)
      val (padded, log) = padTwo1D(complex)
      val transformed = fft1D(padded, log)
      assert(ifft1D(transformed, log) =~= padded)
    }
  }

  test("fft 1D convolution") {
    def convInt(array: IndexedSeq[Int], pattern: IndexedSeq[Int], valid: Boolean = true): IndexedSeq[Int] =
      convolution1D(array, pattern, valid).map(Math.round).map(_.toInt)

    assert(convInt(IndexedSeq(2), IndexedSeq(3)) == IndexedSeq(6))
    assert(convInt(IndexedSeq(4, 5, 6), IndexedSeq(2, 3)) == IndexedSeq(23, 28))
    assert(convInt(IndexedSeq(1, 2, 3), IndexedSeq(2)) == IndexedSeq(2, 4, 6))
    assert(convInt(IndexedSeq(1, 2, 3, 4, 5), IndexedSeq(2, 3, 4)) == IndexedSeq(20, 29, 38))
    assert(convInt(IndexedSeq(2, 3), IndexedSeq(4, 5, 6, 7), valid = false) == IndexedSeq(14, 33, 28, 23, 12))
    assert(convInt(IndexedSeq(1, 2, 3), IndexedSeq(5, 6, 7), valid = false) == IndexedSeq(7, 20, 38, 28, 15))
  }

  test("fft 2D") {
    assert(fft2D(IndexedSeq.empty, -1, -1) =~= IndexedSeq.empty)
    val array = IndexedSeq(IndexedSeq(1, 2), IndexedSeq(3, 4))
    assert(fft2D(array.map(_.map(Complex(_))), 1, 1) =~= IndexedSeq(IndexedSeq(Complex(10), Complex(-2)), IndexedSeq(Complex(-4), Complex(0))))

    val random = new Random(2)
    for(_ <- 0 until 100) {
      val (m, n) = (1 + random.nextInt(10), 1 + random.nextInt(10))
      val array = IndexedSeq.fill(m)(IndexedSeq.fill(n)(random.nextInt(50 + 1) - 25))
      val complex = array.map(_.map(Complex(_)))
      val (padded, logm, logn) = padTwo2D(complex)
      val transformed = fft2D(padded, logm, logn)
      assert(ifft2D(transformed, logm, logn) =~= padded)
    }
  }

  test("fft 2D convolution") {
    def convInt(array: IndexedSeq[IndexedSeq[Int]], pattern: IndexedSeq[IndexedSeq[Int]], valid: Boolean = true): IndexedSeq[IndexedSeq[Int]] =
      convolution2D(array, pattern, valid).map(_.map(Math.round).map(_.toInt))

    val array = IndexedSeq(IndexedSeq(1, 2, 3), IndexedSeq(4, 5, 6), IndexedSeq(7, 8, 9))
    val pattern = IndexedSeq(IndexedSeq(1, 2), IndexedSeq(3, 4))
    assert(convInt(array, pattern) == IndexedSeq(IndexedSeq(37, 47), IndexedSeq(67, 77)))
    assert(convInt(array, pattern, valid = false) == IndexedSeq(IndexedSeq(4, 11, 18, 9), IndexedSeq(18, 37, 47, 21), IndexedSeq(36, 67, 77, 33), IndexedSeq(14, 23, 26, 9)))
  }

}
