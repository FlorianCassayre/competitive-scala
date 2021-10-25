package competitivescala.numbers

object FFT {

  case class Complex(r: Double, i: Double = 0) {
    def *(that: Complex): Complex = Complex(r * that.r - i * that.i, r * that.i + i * that.r)
    def +(that: Complex): Complex = Complex(r + that.r, i + that.i)
    def -(that: Complex): Complex = Complex(r - that.r, i - that.i)
    def exp: Complex = {
      val er = Math.exp(r)
      Complex(er * Math.cos(i), er * Math.sin(i))
    }
  }
  object Complex {
    def from[N](seq: IndexedSeq[N])(implicit ev: Numeric[N]): IndexedSeq[Complex] = seq.map(v => Complex(ev.toDouble(v)))
  }
  val Zero: Complex = Complex(0)

  def fft(array: IndexedSeq[Complex], log2: Int): IndexedSeq[Complex] = {
    if(array.nonEmpty) {
      val n = 1 << log2
      val initial = IndexedSeq.tabulate(n)(i => array(Integer.reverse(i) >>> (Integer.SIZE - log2)))
      (1 to log2).foldLeft(initial) { (tr1, s) =>
        val m = 1 << s
        val m2 = m >> 1
        val a = Math.PI / m2
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
      array
    }
  }

  def ifft(array: IndexedSeq[Complex], log2n: Int): IndexedSeq[Complex] = {
    val n = 1 << log2n
    fft(array.map(c => Complex(c.r, -c.i)), log2n).map(c => Complex(c.r / n, c.i / n))
  }

  def pad[A](array: IndexedSeq[A], m: Int, a: A): IndexedSeq[A] = {
    val n = array.size
    require(n <= m)
    array ++ IndexedSeq.fill(m - n)(a)
  }
  def pad[A](array: IndexedSeq[A], a: A, min: Int = 0): (IndexedSeq[A], Int) = {
    if(array.sizeIs > 1 || min > 1) {
      def log(n: Int, acc: Int): Int = if (n > 1) log(n >>> 1, acc + 1) else acc
      val n = array.size
      val log2n = log(Math.max(n, min) - 1, 1)
      val m = 1 << log2n
      (pad(array, m, a), log2n)
    } else {
      (array, array.size - 1)
    }
  }

  def fft(array: IndexedSeq[Complex]): IndexedSeq[Complex] = {
    val (padded, log2n) = pad(array, Zero)
    fft(padded, log2n)
  }

  def convolution1D[N](array: IndexedSeq[N], pattern: IndexedSeq[N], valid: Boolean = true)(implicit ev: Numeric[N]): IndexedSeq[Double] = {
    val (m, n) = (array.size, pattern.size)
    val size = if(valid) m else m + n - 1
    require(size > 0 && n > 0)
    val (paddedArray, log2n) = pad(Complex.from(array.map(ev.toDouble)), Zero, size)
    val paddedPattern = pad(Complex.from(pattern.reverse.map(ev.toDouble)), 1 << log2n, Zero)
    val (arrayFourier, paddedFourier) = (fft(paddedArray), fft(paddedPattern))
    val product = arrayFourier.zip(paddedFourier).map { case (a, p) => a * p }
    val result = ifft(product, log2n).map(_.r)
    if(valid) result.slice(n - 1, m) else result.take(m + n - 1)
  }

}
