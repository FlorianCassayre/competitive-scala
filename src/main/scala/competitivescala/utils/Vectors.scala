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
    def euclidean: N = x.abs + y.abs
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
  }

  type Vec2Int = Vec2[Int]
  def Vec2Int(x: Int = 0, y: Int = 0): Vec2Int = Vec2(x, y)


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
    def euclidean: N = x.abs + y.abs + z.abs
    def sum: N = x + y + z
    def dot(that: Vec3[N]): N = (this * that).sum
    def sq: N = this.dot(this)
    def cross(that: Vec3[N]): Vec3[N] = Vec3(y * that.z - z * that.y, z * that.x - x * that.z, x * that.y - y * that.x)
  }

  type Vec3Int = Vec3[Int]
  def Vec3Int(x: Int = 0, y: Int = 0, z: Int = 0): Vec3Int = Vec3(x, y, z)


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
    def euclidean: N = x.abs + y.abs + z.abs + w.abs
    def sum: N = x + y + z + w
    def dot(that: Vec4[N]): N = (this * that).sum
    def sq: N = this.dot(this)
  }

  type Vec4Int = Vec4[Int]
  def Vec4Int(x: Int = 0, y: Int = 0, z: Int = 0, w: Int = 0): Vec4Int = Vec4(x, y, z, w)

}
