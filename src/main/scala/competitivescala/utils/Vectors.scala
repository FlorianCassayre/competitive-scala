package competitivescala.utils

object Vectors {

  case class Vec2[N](x: N, y: N)(implicit ev: Integral[N]) {
    import ev._
    def pointwise(f: (N, N) => N)(that: Vec2[N]): Vec2[N] = Vec2(f(x, that.x), f(y, that.y))
    def map(f: N => N): Vec2[N] = Vec2(f(x), f(y))
    def +(that: Vec2[N]): Vec2[N] = pointwise(_ + _)(that)
    def -(that: Vec2[N]): Vec2[N] = pointwise(_ - _)(that)
    def *(that: Vec2[N]): Vec2[N] = pointwise(_ * _)(that)
    def /(that: Vec2[N]): Vec2[N] = pointwise(_ / _)(that)
    def *(v: N): Vec2[N] = map(_ * v)
    def /(v: N): Vec2[N] = map(_ / v)
    def unary_- : Vec2[N] = map(-_)
    def manhattan: N = x.abs + y.abs
    def sum: N = x + y
    def dot(that: Vec2[N]): N = (this * that).sum
    def sq: N = this.dot(this)
    def cross(that: Vec2[N]): N = x * that.y - y * that.x
    def swap: Vec2[N] = Vec2(y, x)
    private def quadrant(i: Int): Int = ((i % 4) + 4) % 4
    def clockwise(i: Int = 1): Vec2[N] = {
      def iterate(i: Int, vec: Vec2[N]): Vec2[N] = if (i > 0) iterate(i - 1, Vec2(vec.y, -vec.x)) else vec
      iterate(quadrant(i), this)
    }
    def counterClockwise(i: Int = 1): Vec2[N] = clockwise(-i)
    def >>(that: Vec2[N]): Boolean = x > that.x && y > that.y
    def <<(that: Vec2[N]): Boolean = this >> that
    def >>=(that: Vec2[N]): Boolean = x >= that.x && y >= that.y
    def <<=(that: Vec2[N]): Boolean = that >>= this
    def min(that: Vec2[N]): Vec2[N] = pointwise(ev.min)(that)
    def max(that: Vec2[N]): Vec2[N] = pointwise(ev.max)(that)
  }

  type Vec2Int = Vec2[Int]
  def Vec2Int(x: Int = 0, y: Int = 0): Vec2Int = Vec2(x, y)

    def directions2[N](implicit ev: Integral[N]): Seq[Vec2[N]] =
      Seq.fill(2)(Seq(ev.negate(ev.one), ev.one)).flatten.combinations(2).flatMap(_.permutations).map(t => Vec2(t(0), t(1))).toSeq


  case class Vec3[N](x: N, y: N, z: N)(implicit ev: Integral[N]) {
    import ev._
    def pointwise(f: (N, N) => N)(that: Vec3[N]): Vec3[N] = Vec3(f(x, that.x), f(y, that.y), f(z, that.z))
    def map(f: N => N): Vec3[N] = Vec3(f(x), f(y), f(z))
    def +(that: Vec3[N]): Vec3[N] = pointwise(_ + _)(that)
    def -(that: Vec3[N]): Vec3[N] = pointwise(_ - _)(that)
    def *(that: Vec3[N]): Vec3[N] = pointwise(_ * _)(that)
    def /(that: Vec3[N]): Vec3[N] = pointwise(_ / _)(that)
    def *(v: N): Vec3[N] = map(_ * v)
    def /(v: N): Vec3[N] = map(_ / v)
    def unary_- : Vec3[N] = map(-_)
    def manhattan: N = x.abs + y.abs + z.abs
    def sum: N = x + y + z
    def dot(that: Vec3[N]): N = (this * that).sum
    def sq: N = this.dot(this)
    def cross(that: Vec3[N]): Vec3[N] = Vec3(y * that.z - z * that.y, z * that.x - x * that.z, x * that.y - y * that.x)
    def >>(that: Vec3[N]): Boolean = x > that.x && y > that.y && z > that.z
    def <<(that: Vec3[N]): Boolean = that >> this
    def >>=(that: Vec3[N]): Boolean = x >= that.x && y >= that.y && z >= that.z
    def <<=(that: Vec3[N]): Boolean = that >>= this
    def min(that: Vec3[N]): Vec3[N] = pointwise(ev.min)(that)
    def max(that: Vec3[N]): Vec3[N] = pointwise(ev.max)(that)
  }

  type Vec3Int = Vec3[Int]
  def Vec3Int(x: Int = 0, y: Int = 0, z: Int = 0): Vec3Int = Vec3(x, y, z)

  def directions3[N](implicit ev: Integral[N]): Seq[Vec3[N]] =
    Seq.fill(3)(Seq(ev.negate(ev.one), ev.one)).flatten.combinations(3).flatMap(_.permutations).map(t => Vec3(t(0), t(1), t(2))).toSeq


  case class Vec4[N](x: N, y: N, z: N, w: N)(implicit ev: Integral[N]) {
    import ev._
    def pointwise(f: (N, N) => N)(that: Vec4[N]): Vec4[N] = Vec4(f(x, that.x), f(y, that.y), f(z, that.z), f(w, that.w))
    def map(f: N => N): Vec4[N] = Vec4(f(x), f(y), f(z), f(w))
    def +(that: Vec4[N]): Vec4[N] = pointwise(_ + _)(that)
    def -(that: Vec4[N]): Vec4[N] = pointwise(_ - _)(that)
    def *(that: Vec4[N]): Vec4[N] = pointwise(_ * _)(that)
    def /(that: Vec4[N]): Vec4[N] = pointwise(_ / _)(that)
    def *(v: N): Vec4[N] = map(_ * v)
    def /(v: N): Vec4[N] = map(_ / v)
    def unary_- : Vec4[N] = map(-_)
    def manhattan: N = x.abs + y.abs + z.abs + w.abs
    def sum: N = x + y + z + w
    def dot(that: Vec4[N]): N = (this * that).sum
    def sq: N = this.dot(this)
    def >>(that: Vec4[N]): Boolean = x > that.x && y > that.y && z > that.z && w > that.w
    def <<(that: Vec4[N]): Boolean = that >> this
    def >>=(that: Vec4[N]): Boolean = x >= that.x && y >= that.y && z >= that.z && w >= that.w
    def <<=(that: Vec4[N]): Boolean = that >>= this
    def min(that: Vec4[N]): Vec4[N] = pointwise(ev.min)(that)
    def max(that: Vec4[N]): Vec4[N] = pointwise(ev.max)(that)
  }

  type Vec4Int = Vec4[Int]
  def Vec4Int(x: Int = 0, y: Int = 0, z: Int = 0, w: Int = 0): Vec4Int = Vec4(x, y, z, w)

  def directions4[N](implicit ev: Integral[N]): Seq[Vec4[N]] =
    Seq.fill(4)(Seq(ev.negate(ev.one), ev.one)).flatten.combinations(4).flatMap(_.permutations).map(t => Vec4(t(0), t(1), t(2), t(3))).toSeq

  // Axis-aligned bounding boxes (AABB)

  case class AABB2[N] private (min: Vec2[N], max: Vec2[N])(implicit ev: Integral[N]) {
    require(min <<= max)
    def |(that: AABB2[N]): AABB2[N] = new AABB2(min.min(that.min), max.max(that.max))
    def &(that: AABB2[N]): Option[AABB2[N]] = {
      val (nmin, nmax) = (min.max(that.min), max.min(that.max))
      if(nmin <<= nmax) Some(new AABB2(nmin, nmax)) else None
    }
    def contains(p: Vec2[N]): Boolean = (p >>= min) && (p <<= max)
  }
  object AABB2 {
    def apply[N](p1: Vec2[N], p2: Vec2[N])(implicit ev: Integral[N]): AABB2[N] = new AABB2(p1.min(p2), p1.max(p2))
  }


  case class AABB3[N] private (min: Vec3[N], max: Vec3[N])(implicit ev: Integral[N]) {
    require(min <<= max)
    def |(that: AABB3[N]): AABB3[N] = new AABB3(min.min(that.min), max.max(that.max))
    def &(that: AABB3[N]): Option[AABB3[N]] = {
      val (nmin, nmax) = (min.max(that.min), max.min(that.max))
      if(nmin <<= nmax) Some(new AABB3(nmin, nmax)) else None
    }
    def contains(p: Vec3[N]): Boolean = (p >>= min) && (p <<= max)
  }
  object AABB3 {
    def apply[N](p1: Vec3[N], p2: Vec3[N])(implicit ev: Integral[N]): AABB3[N] = new AABB3(p1.min(p2), p1.max(p2))
  }


  case class AABB4[N] private (min: Vec4[N], max: Vec4[N])(implicit ev: Integral[N]) {
    require(min <<= max)
    def |(that: AABB4[N]): AABB4[N] = new AABB4(min.min(that.min), max.max(that.max))
    def &(that: AABB4[N]): Option[AABB4[N]] = {
      val (nmin, nmax) = (min.max(that.min), max.min(that.max))
      if(nmin <<= nmax) Some(new AABB4(nmin, nmax)) else None
    }
    def contains(p: Vec4[N]): Boolean = (p >>= min) && (p <<= max)
  }
  object AABB4 {
    def apply[N](p1: Vec4[N], p2: Vec4[N])(implicit ev: Integral[N]): AABB4[N] = new AABB4(p1.min(p2), p1.max(p2))
  }

}
