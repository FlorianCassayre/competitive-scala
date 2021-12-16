package competitivescala.utils

// Inspired by https://en.wikipedia.org/wiki/Hilbert_curve

object SpaceFillingCurves {

  // Side length: 2 << n
  // Area: (1 << 2) << (n << 1)

  // [n = 2]
  // 0 2
  // 1 3
  // [n = 3]
  // 0 2 8 A
  // 1 3 9 B
  // 4 6 C E
  // 5 7 D F
  def binaryFromIndex(n: Int)(i: Int): (Int, Int) = {
    def toInt(seq: Seq[Int]): Int = seq.foldLeft(0)((acc, e) => (acc << 1) | e)
    def iterate(k: Int, i: Int, x: Seq[Int], y: Seq[Int]): (Int, Int) =
      if(k >= 0) iterate(k - 1, i >> 2, ((i >> 1) & 1) +: x, (i & 1) +: y) else (toInt(x), toInt(y))
    iterate(n, i, Seq.empty, Seq.empty)
  }

  def binaryToIndex(n: Int)(x: Int, y: Int): Int = {
    def iterate(k: Int, i: Seq[Int], x: Int, y: Int): Int =
      if(k >= 0) iterate(k - 1, (x & 1) +: (y & 1) +: i, x >> 1, y >> 1) else i.foldLeft(0)((acc, e) => (acc << 1) | e)
    iterate(n, Seq.empty, x, y)
  }

  // [n = 2]
  // 0 3
  // 1 2
  // [n = 3]
  // 0 3 F C
  // 1 2 E D
  // 7 4 8 B
  // 6 5 9 A
  def grayCodeFromIndex(n: Int)(i: Int): (Int, Int) = binaryFromIndex(n)(i ^ (i >> 1))

  def grayCodeToIndex(n: Int)(x: Int, y: Int): Int = {
    def grayInverse(i: Int, n: Int): Int = if(i != 0) grayInverse(i >> 1, n ^ i) else n
    grayInverse(binaryToIndex(n)(x, y), 0)
  }


  def hilbertRotate(k: Int, rx: Int, ry: Int, x: Int, y: Int): (Int, Int) =
    if(ry == 0) if(rx == 1) (k - 1 - y, k - 1 - x) else (y, x) else (x, y)

  // [n = 0]
  // 0 3
  // 1 2
  // [n = 1]
  // 0 1 E F
  // 3 2 D C
  // 4 7 8 B
  // 5 6 9 A
  def hilbertFromIndex(n: Int)(i: Int): (Int, Int) = {
    val k = 2 << n
    def iterate(x: Int, y: Int, s: Int, t: Int): (Int, Int) =
      if(s < k) {
        val rx = (t >> 1) & 1
        val ry = (t ^ rx) & 1
        val (nx, ny) = hilbertRotate(s, rx, ry, x, y)
        iterate(nx + s * rx, ny + s * ry, s << 1, t >> 2)
      } else (x, y)
    iterate(0, 0, 1, i)
  }

  def hilbertToIndex(n: Int)(x: Int, y: Int): Int = {
    val k = 2 << n
    def iterate(x: Int, y: Int, s: Int, d: Int): Int = {
      if(s > 0) {
        val (rx, ry) = (if((x & s) > 0) 1 else 0, if((y & s) > 0) 1 else 0)
        val nd = d + s * s * ((3 * rx) ^ ry)
        val (nx, ny) = hilbertRotate(k, rx, ry, x, y)
        iterate(nx, ny, s >> 1, nd)
      } else d
    }
    iterate(x, y, k >> 1, 0)
  }

}
