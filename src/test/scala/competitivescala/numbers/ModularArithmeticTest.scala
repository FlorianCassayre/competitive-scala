package competitivescala.numbers

import org.scalatest.funsuite.AnyFunSuite

import ModularArithmetic._

class ModularArithmeticTest extends AnyFunSuite {

  test("greatest common divisor") {
    assert(gcd(4, 12) == 4)
    assert(gcd(30, 24) == 6)
    assert(gcd(97, 32) == 1)
    assert(gcd(13, 23) == 1)
    assert(gcd(1, 3) == 1)
    assert(gcd(3, 1) == 1)
    assert(gcd(0, 3) == 3)
    assert(gcd(3, 0) == 3)
    assert(gcd(0, 0) == 0)
    assert(gcd(-6, 8) == 2)
    assert(gcd(-10, -2) == 2)
  }

  test("least common multiplier") {
    assert(lcm(7, 9) == 63)
    assert(lcm(6, 15) == 30)
    assert(lcm(4, 12) == 12)
    assert(lcm(3, 0) == 0)
    assert(lcm(0, 0) == 0)
    assert(lcm(3, -6) == 6)
    assert(lcm(-4, -2) == 4)
  }

  test("bezout identity") {
    assert(gcdBezout(5, 3) == (1, (-1, 2)))
    assert(gcdBezout(9, 11) == (1, (5, -4)))
    assert(gcdBezout(18, 25) == (1, (7, -5)))
    assert(gcdBezout(16, 12) == (4, (1, -1)))
  }

  test("modular inverse") {
    assert(modularInverse(2, 7).contains(4))
    assert(modularInverse(5, 9).contains(2))
    assert(modularInverse(28, 53).contains(36))
    assert(modularInverse(2, 12).isEmpty)
    assert(modularInverse(0, 13).isEmpty)
    assert(modularInverse(1, 15).contains(1))
  }

  test("chinese remainder") {
    assert(chineseRemainder(Seq((10, 5))).contains(5))
    assert(chineseRemainder(Seq((5, 3), (4, 1))).contains(13))
    assert(chineseRemainder(Seq((3, 2), (5, 3), (7, 2))).contains(23))
    assert(chineseRemainder(Seq((12, 5), (9, 7))).isEmpty)
  }

  test("fast exponentation") {
    assert(exponent(3, 0, 10) == 1)
    assert(exponent(3, 1, 10) == 3)
    assert(exponent(3, 2, 10) == 9)
    assert(exponent(3, 3, 10) == 7)
    assert(exponent(2, 10, 13) == 10)
    assert(exponent(37, 1536, 877) == 664)
  }

  test("sum of powers") {
    assert(sumPowers(5, 0, 13) == 1)
    assert(sumPowers(7, 1, 97) == 8)
    assert(sumPowers(9, 2, 97) == 91)
    assert(sumPowers(2, 5, 100) == 63)
    assert(sumPowers(3, 6, 1001) == 92)
  }

  test("positive modulo") {
    assert(positiveMod(12, 7) == 5)
    assert(positiveMod(0, 9) == 0)
    assert(positiveMod(-3, 8) == 5)
    assert(positiveMod(-51, 16) == 13)
  }

  test("miller-rabin primality test") {
    def isPrime(n: BigInt): Boolean = millerRabinPrimalityTestBigInt(n, 200)
    assert(!isPrime(BigInt(1)))
    assert(isPrime(BigInt(2)))
    assert(isPrime(BigInt(3)))
    assert(!isPrime(BigInt(4)))
    assert(isPrime(BigInt(5)))
    assert(isPrime(BigInt(41)))
    assert(!isPrime(BigInt(42)))
    assert(isPrime(BigInt(43)))
    assert(isPrime(BigInt(97)))
    assert(!isPrime(BigInt("21897574116239148563")))
    assert(isPrime(BigInt("662907566987488772370319039045404693994847")))
  }

  test("ceiled square root") {
    assert(ceiledSqrt(0) == 0)
    assert(ceiledSqrt(1) == 1)
    assert(ceiledSqrt(2) == 2)
    assert(ceiledSqrt(3) == 2)
    assert(ceiledSqrt(4) == 2)
    assert(ceiledSqrt(5) == 3)
    assert(ceiledSqrt(50) == 8)
    assert(ceiledSqrt(99) == 10)
  }

  test("baby-step giant-step") {
    assert(babyStepGiantStep(0, 0, 1).contains(0))
    assert(babyStepGiantStep(0, 0, 2).isEmpty)
    assert(babyStepGiantStep(2, 0, 3).isEmpty)
    assert(babyStepGiantStep(4, 2, 5).isEmpty)
    assert(babyStepGiantStep(2, 3, 5).contains(3))
    assert(babyStepGiantStep(2, 3, 11).contains(8))
    assert(babyStepGiantStep(2, 3, 11).contains(8))
    assert(babyStepGiantStep(32174, 49111, 59351).contains(24059))
    assert(babyStepGiantStep(227633941L, 29051987L, 83932133L).contains(34562833L))
  }

}
