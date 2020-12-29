package utils

object Vectors {

  case class Vec2(x: Int = 0, y: Int = 0) {
    def pointwise(f: (Int, Int) => Int)(that: Vec2): Vec2 = Vec2(f(x, that.x), f(y, that.y))
    def map(f: Int => Int): Vec2 = Vec2(f(x), f(y))
    def +(that: Vec2): Vec2 = pointwise(_ + _)(that)
    def -(that: Vec2): Vec2 = pointwise(_ - _)(that)
    def *(that: Vec2): Vec2 = pointwise(_ * _)(that)
    def /(that: Vec2): Vec2 = pointwise(_ / _)(that)
    def *(v: Int): Vec2 = map(_ * v)
    def /(v: Int): Vec2 = map(_ / v)
    def unary_- : Vec2 = map(-_)
    def euclidean: Int = x.abs + y.abs
    def sum: Int = x + y
    def dot(that: Vec2): Int = (this * that).sum
    def sq: Int = this.dot(this)
    def cross(that: Vec2): Int = x * that.y - y * that.x
    def swap: Vec2 = Vec2(y, x)
    private def quadrant(i: Int): Int = ((i % 4) + 4) % 4
    def clockwise(i: Int = 1): Vec2 = {
      def iterate(i: Int, vec: Vec2): Vec2 = if (i > 0) iterate(i - 1, Vec2(vec.y, -vec.x)) else vec
      iterate(quadrant(i), this)
    }
    def counterClockwise(i: Int = 1): Vec2 = clockwise(-i)
  }

  case class Vec3(x: Int = 0, y: Int = 0, z: Int = 0) {
    def pointwise(f: (Int, Int) => Int)(that: Vec3): Vec3 = Vec3(f(x, that.x), f(y, that.y), f(z, that.z))
    def map(f: Int => Int): Vec3 = Vec3(f(x), f(y), f(z))
    def +(that: Vec3): Vec3 = pointwise(_ + _)(that)
    def -(that: Vec3): Vec3 = pointwise(_ - _)(that)
    def *(that: Vec3): Vec3 = pointwise(_ * _)(that)
    def /(that: Vec3): Vec3 = pointwise(_ / _)(that)
    def *(v: Int): Vec3 = map(_ * v)
    def /(v: Int): Vec3 = map(_ / v)
    def unary_- : Vec3 = map(-_)
    def euclidean: Int = x.abs + y.abs + z.abs
    def sum: Int = x + y + z
    def dot(that: Vec3): Int = (this * that).sum
    def sq: Int = this.dot(this)
    def cross(that: Vec3): Vec3 = Vec3(y * that.z - z * that.y, z * that.x - x * that.z, x * that.y - y * that.x)
  }

  case class Vec4(x: Int = 0, y: Int = 0, z: Int = 0, w: Int = 0) {
    def pointwise(f: (Int, Int) => Int)(that: Vec4): Vec4 = Vec4(f(x, that.x), f(y, that.y), f(z, that.z), f(w, that.w))
    def map(f: Int => Int): Vec4 = Vec4(f(x), f(y), f(z), f(w))
    def +(that: Vec4): Vec4 = pointwise(_ + _)(that)
    def -(that: Vec4): Vec4 = pointwise(_ - _)(that)
    def *(that: Vec4): Vec4 = pointwise(_ * _)(that)
    def /(that: Vec4): Vec4 = pointwise(_ / _)(that)
    def *(v: Int): Vec4 = map(_ * v)
    def /(v: Int): Vec4 = map(_ / v)
    def unary_- : Vec4 = map(-_)
    def euclidean: Int = x.abs + y.abs + z.abs + w.abs
    def sum: Int = x + y + z + w
    def dot(that: Vec4): Int = (this * that).sum
    def sq: Int = this.dot(this)
  }

}
