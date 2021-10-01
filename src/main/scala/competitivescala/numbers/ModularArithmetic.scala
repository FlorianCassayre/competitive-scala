package competitivescala.numbers

object ModularArithmetic {

  def gcd[N](a: N, b: N)(implicit ev: Integral[N]): N = {
    import ev._
    if (b == zero) a.abs else gcd(b, a % b)
  }

  def lcm[N](a: N, b: N)(implicit ev: Integral[N]): N = {
    import ev._
    val p = (a * b).abs
    if (p == zero) p else p / gcd(a, b)
  }


  // Returns (gcd(a, b), (u, v)) s.t. gcd(a, b) = a*u + b*v
  def gcdBezout[N](a: N, b: N)(implicit ev: Integral[N]): (N, (N, N)) = {
    import ev._
    def iterate(rn: N, r: N, sn: N, s: N, tn: N, t: N): (N, (N, N)) = {
      if (r != zero) {
        val q = rn / r
        iterate(r, rn - q * r, s, sn - q * s, t, tn - q * t)
      } else {
        (rn, (sn, tn))
      }
    }
    iterate(a, b, one, zero, zero, one)
  }


  // Simpler version of the above
  def modularInverse[N](a: N, m: N)(implicit ev: Integral[N]): Option[N] = {
    import ev._
    def iterate(tn: N, t: N, rn: N, r: N): Option[N] = {
      if (r != zero) {
        val q = rn / r
        iterate(t, tn - q * t, r, rn - q * r)
      } else {
        if (rn > one) None else Some(if (tn < zero) tn + m else tn)
      }
    }
    iterate(zero, one, m, a)
  }

  // Solves for x: x = a(i) (mod n(i))
  def chineseRemainder[N](na: Seq[(N, N)])(implicit ev: Integral[N]): Option[N] = {
    import ev._
    val p = na.map(_._1).product
    def iterate(na: Seq[(N, N)], x: N): Option[N] = na match {
      case (n, a) +: tail =>
        val ni = p / n
        modularInverse(ni, n) match {
          case Some(vi) => iterate(tail, (x + a * vi * ni) % p)
          case None => None
        }
      case _ => Some(x)
    }
    iterate(na, zero)
  }


  // Computes a^b mod m
  def exponent[N](a: N, b: N, m: N)(implicit ev: Integral[N]): N = {
    import ev._
    val two = one + one
    def iterate(y: N, x: N, n: N): N = {
      if (n < zero) throw new Exception // iterate(y, modularInverse(x, m).get, -n)
      else if (n == zero) y
      else if (n == one) (x * y) % m
      else if (n % two == zero) iterate(y, (x * x) % m, n / two)
      else iterate((x * y) % m, (x * x) % m, n / two)
    }
    iterate(one, a % m, b)
  }

  // Computes a^0+a^1+a^2+...+a^k mod m
  def sumPowers[N](a: N, k: N, m: N)(implicit ev: Integral[N]): N = {
    import ev._
    ((exponent(a, k + one, m) - one) * modularInverse(a - one, m).get) % m
  }

  // If false then the number is surely composite
  // If true then it is prime with probability guaranteed at least 1 - 4^-k
  def millerRabinPrimalityTest[N](n: N, k: Int)(randomSource: N => () => N)(implicit ev: Integral[N]): Boolean = {
    import ev._
    require(n > zero)
    require(k > 0)
    val (two, three) = (fromInt(2), fromInt(3))
    val nMinusOne = n - one
    def factorTwo(number: N, count: N): (N, N) = {
      if(number > zero && number % two == zero) {
        factorTwo(number / two, count + one)
      } else {
        (number, count)
      }
    }
    val (d, r) = factorTwo(n - one, zero)
    val randomGenerator = randomSource(n - three)
    def witness(i: Int): Boolean = {
      if(i > 0) {
        val a = randomGenerator() + two
        val x = exponent(a, d, n)
        if(x == one || x == nMinusOne) {
          witness(i - 1)
        } else {
          def repeat(j: N, y: N): Boolean = {
            if(j > one) { // r - 1 iterations
              val y1 = exponent(y, two, n)
              if(y1 == nMinusOne) {
                true
              } else {
                repeat(j - one, y1)
              }
            } else {
              false
            }
          }
          if(repeat(r, x)) {
            witness(i - 1)
          } else {
            false
          }
        }
      } else {
        true
      }
    }
    if(n <= one) {
      false
    } else if(n <= three) {
      true
    } else if(n % two == zero || n % three == zero) { // Optional test
      false
    } else {
      witness(k)
    }
  }

  def millerRabinPrimalityTestBigInt(n: BigInt, k: Int): Boolean = {
    import scala.util.Random
    val random = new Random()
    def nextRandom(number: BigInt, bitLength: Int): BigInt = {
      val next = BigInt(bitLength, random)
      if(next < number) {
        next
      } else {
        nextRandom(number, bitLength)
      }
    }
    millerRabinPrimalityTest(n, k)(number => {
      val bitLength = number.bitLength
      () => nextRandom(number, bitLength)
    })
  }

  def millerRabinPrimalityTestLong(n: Long, k: Int): Boolean = {
    import scala.util.Random
    val random = new Random()
    millerRabinPrimalityTest(n, k)(number => () => random.nextLong(number))
  }


  // Finds the smallest integer that is greater or equal to sqrt(n)
  def ceiledSqrt[N](n: N)(implicit ev: Integral[N]): N = {
    import ev._
    require(n >= zero)
    val two = one + one
    def find(start: N, end: N, found: N): N = {
      if(start <= end) {
        val mid = (start + end) / two
        val value = mid * mid
        if(value >= n) {
          find(start, mid - one, mid)
        } else {
          find(mid + one, end, found)
        }
      } else {
        found
      }
    }
    find(zero, n, n)
  }

  // Complexity and memory: Õ(sqrt(n))
  // Finds x such that a^x = b
  def babyStepGiantStep[N](a: N, b: N, n: N)(implicit ev: Integral[N]): Option[N] = {
    import ev._
    val m = ceiledSqrt(n)
    val table = Seq.unfold(zero, one % n) { case (j, acc) => if(j < m) Some(acc -> j, (j + one, (acc * a) % n)) else None }.toMap
    modularInverse(a, n).flatMap { ai =>
      val am = exponent(ai, m, n)
      def iterate(i: N, y: N): Option[N] = {
        if(i < m) {
          table.get(y) match {
            case Some(j) => Some(i * m + j)
            case None => iterate(i + one, (y * am) % n)
          }
        } else {
          None
        }
      }
      iterate(zero, b % n)
    }
  }


  def positiveMod[N](a: N, m: N)(implicit ev: Integral[N]): N = {
    import ev._
    ((a % m) + m) % m
  }

}
