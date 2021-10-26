package competitivescala.numbers

object FFT {

  case class Complex(r: Double, i: Double = 0) {
    def *(that: Complex): Complex = Complex(r * that.r - i * that.i, r * that.i + i * that.r)
    def +(that: Complex): Complex = Complex(r + that.r, i + that.i)
    def -(that: Complex): Complex = Complex(r - that.r, i - that.i)
    def *(v: Double): Complex = Complex(r * v, i * v)
    def exp: Complex = {
      val er = Math.exp(r)
      Complex(er * Math.cos(i), er * Math.sin(i))
    }
  }
  object Complex {
    def from[N](seq: IndexedSeq[N])(implicit ev: Numeric[N]): IndexedSeq[Complex] = seq.map(v => Complex(ev.toDouble(v)))
  }
  val Zero: Complex = Complex(0)

  def log2(n: Int): Int = {
    def log2(n: Int, acc: Int): Int = if (n > 0) log2(n >>> 1, acc + 1) else acc
    if(n > 0) log2(n - 1, 0) else -1
  }
  def pad[A](array: IndexedSeq[A], m: Int, a: A): IndexedSeq[A] = {
    val n = array.size
    require(n <= m)
    array ++ IndexedSeq.fill(m - n)(a)
  }
  def padTwo[A](array: IndexedSeq[A], a: A, min: Int = 0): (IndexedSeq[A], Int) = {
    val log2n = log2(Math.max(array.size, min))
    (if(log2n >= 0) pad(array, 1 << log2n, a) else array, log2n)
  }
  def padTwo1D(array: IndexedSeq[Complex], min: Int = 0): (IndexedSeq[Complex], Int) =
    padTwo(array, Zero, min)
  def padTwo2D(array: IndexedSeq[IndexedSeq[Complex]], minm: Int = 0, minn: Int = 0): (IndexedSeq[IndexedSeq[Complex]], Int, Int) = {
    if(array.nonEmpty) {
      val (m, n) = (array.size, array.head.size)
      val (log2m, log2n) = (log2(Math.max(m, minm)), log2(Math.max(n, minn)))
      val padded = IndexedSeq.tabulate(1 << log2m, 1 << log2n)((i, j) => if(i < m && j < n) array(i)(j) else Zero)
      (padded, log2m, log2n)
    } else {
      (array, -1, -1)
    }
  }

  def fft1D(array: IndexedSeq[Complex], log2n: Int): IndexedSeq[Complex] = {
    if(array.nonEmpty) {
      val n = 1 << log2n
      require(array.size == n)
      val initial = IndexedSeq.tabulate(n)(i => array(Integer.reverse(i) >>> (Integer.SIZE - log2n)))
      (1 to log2n).foldLeft(initial) { (tr1, s) =>
        val m = 1 << s
        val m2 = m >> 1
        val a = -Math.PI / m2
        val wm = Complex(Math.cos(a), Math.sin(a))
        (0 until m2).foldLeft((tr1, Complex(1))) { case ((tr1, w), j) =>
          ((j until n by m).foldLeft(tr1) { (tr2, k) =>
            val t = w * tr2(k + m2)
            val u = tr2(k)
            tr2.updated(k, u + t).updated(k + m2, u - t)
          }, w * wm)
        }._1
      }
    } else {
      require(log2n == -1)
      array
    }
  }

  def ifft1D(array: IndexedSeq[Complex], log2n: Int): IndexedSeq[Complex] = {
    val n = 1 << log2n
    fft1D(array.map(c => Complex(c.r, -c.i)), log2n).map(c => Complex(c.r / n, c.i / n))
  }

  def convolution1D[N](array: IndexedSeq[N], pattern: IndexedSeq[N], valid: Boolean = true)(implicit ev: Numeric[N]): IndexedSeq[Double] = {
    val (m, n) = (array.size, pattern.size)
    val size = if(valid) m else m + n - 1
    require(size > 0 && n > 0)
    val (paddedArray, log2n) = padTwo1D(Complex.from(array.map(ev.toDouble)), size)
    val paddedPattern = pad(Complex.from(pattern.reverse.map(ev.toDouble)), 1 << log2n, Zero)
    val (fourierArray, fourierPattern) = (fft1D(paddedArray, log2n), fft1D(paddedPattern, log2n))
    val product = fourierArray.zip(fourierPattern).map { case (a, p) => a * p }
    val result = ifft1D(product, log2n).map(_.r)
    if(valid) result.slice(n - 1, m) else result.take(m + n - 1)
  }

  def fft2D(array: IndexedSeq[IndexedSeq[Complex]], log2m: Int, log2n: Int): IndexedSeq[IndexedSeq[Complex]] = {
    if(array.nonEmpty) {
      require(array.head.nonEmpty)
      val (m, n) = (1 << log2m, 1 << log2n)
      require(m == array.size && n == array.head.size)
      array.map(row => fft1D(row, log2n)).transpose.map(column => fft1D(column, log2m)).transpose
    } else {
      array
    }
  }

  def ifft2D(array: IndexedSeq[IndexedSeq[Complex]], log2m: Int, log2n: Int): IndexedSeq[IndexedSeq[Complex]] = {
    val (m, n) = (1 << log2m, 1 << log2n)
    val mn = m * n
    fft2D(array.map(_.map(c => Complex(c.r, -c.i))), log2m, log2n).map(_.map(c => Complex(c.r / mn, c.i / mn)))
  }

  def convolution2D[N](array: IndexedSeq[IndexedSeq[N]], pattern: IndexedSeq[IndexedSeq[N]], valid: Boolean = true)(implicit ev: Numeric[N]): IndexedSeq[IndexedSeq[Double]] = {
    require(array.nonEmpty && pattern.nonEmpty)
    val ((m1, m2), (n1, n2)) = ((array.size, array.head.size), (pattern.size, pattern.head.size))
    val (s1, s2) = if(valid) (m1, m2) else (m1 + n1 - 1, m2 + n2 - 1)
    val (complexArray, complexPattern) = (array.map(r => Complex.from(r.map(ev.toDouble))), pattern.map(r => Complex.from(r.reverse.map(ev.toDouble))).reverse)
    val ((paddedArray, logm, logn), (paddedPattern, _, _)) = (padTwo2D(complexArray, s1, s2), padTwo2D(complexPattern, s1, s2))
    val (fourierArray, fourierPattern) = (fft2D(paddedArray, logm, logn), fft2D(paddedPattern, logm, logn))
    val product = fourierArray.zip(fourierPattern).map { case (r1, r2) => r1.zip(r2).map { case (a, b) => a * b } }
    val result = ifft2D(product, logm, logn).map(_.map(_.r))
    if(valid) result.slice(n1 - 1, m1).map(_.slice(n2 - 1, m2)) else result.take(m1 + n1 - 1).map(_.take(m2 + n2 - 1))
  }

}
