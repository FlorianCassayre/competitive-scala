package competitivescala.numbers

object ModularArithmetic {

  def gcd[N](a: N, b: N)(implicit ev: Integral[N]): N = {
    import ev._
    if (b == ev.zero) a.abs else gcd(b, a % b)
  }

  def lcm[N](a: N, b: N)(implicit ev: Integral[N]): N = {
    import ev._
    val p = (a * b).abs
    if (p == ev.zero) p else p / gcd(a, b)
  }


  // Returns (gcd(a, b), (u, v)) s.t. gcd(a, b) = a*u + b*v
  def gcdBezout[N](a: N, b: N)(implicit ev: Integral[N]): (N, (N, N)) = {
    def iterate(rn: N, r: N, sn: N, s: N, tn: N, t: N): (N, (N, N)) = {
      import ev._
      if (r != ev.zero) {
        val q = rn / r
        iterate(r, rn - q * r, s, sn - q * s, t, tn - q * t)
      } else {
        (rn, (sn, tn))
      }
    }
    iterate(a, b, ev.one, ev.zero, ev.zero, ev.one)
  }


  // Simpler version of the above
  def modularInverse[N](a: N, m: N)(implicit ev: Integral[N]): Option[N] = {
    def iterate(tn: N, t: N, rn: N, r: N): Option[N] = {
      import ev._
      if (r != ev.zero) {
        val q = rn / r
        iterate(t, tn - q * t, r, rn - q * r)
      } else {
        if (rn > ev.one) None else Some(if (tn < ev.zero) tn + m else tn)
      }
    }
    iterate(ev.zero, ev.one, m, a)
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
    iterate(na, ev.zero)
  }


  def exponent[N](a: N, b: N, m: N)(implicit ev: Integral[N]): N = {
    import ev._
    val two = ev.one + ev.one
    def iterate(y: N, x: N, n: N): N = {
      if (n < ev.zero) throw new Exception // iterate(y, modularInverse(x, m).get, -n)
      else if (n == ev.zero) y
      else if (n == ev.one) (x * y) % m
      else if (n % two == ev.zero) iterate(y, (x * x) % m, n / two)
      else iterate((x * y) % m, (x * x) % m, n / two)
    }
    iterate(ev.one, a % m, b)
  }


  def positiveMod[N](a: N, m: N)(implicit ev: Integral[N]): N = {
    import ev._
    ((a % m) + m) % m
  }

}
