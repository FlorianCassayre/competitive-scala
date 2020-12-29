package utils

object HexGrid {

  case class HexPoint(x: Int = 0, y: Int = 0, z: Int = 0) {
    def pointwise(f: (Int, Int) => Int)(that: HexPoint): HexPoint = HexPoint(f(x, that.x), f(y, that.y), f(z, that.z))
    def map(f: Int => Int): HexPoint = HexPoint(f(x), f(y), f(z))
    def +(that: HexPoint): HexPoint = pointwise(_ + _)(that)
    def -(that: HexPoint): HexPoint = pointwise(_ - _)(that)
    def unary_- : HexPoint = map(-_)
    def hexEuclidean: Int = (x.abs + y.abs + z.abs) / 2
  }

  object HexRing {
    val northeast: HexPoint = HexPoint(y = 1, z = -1)
    val east: HexPoint = HexPoint(x = 1, z = -1)
    val southeast: HexPoint = HexPoint(x = 1, y = -1)
    val southwest: HexPoint = HexPoint(y = -1, z = 1)
    val west: HexPoint = HexPoint(x = -1, z = 1)
    val northwest: HexPoint = HexPoint(x = -1, y = 1)

    val directions: Seq[HexPoint] = Seq(northeast, east, southeast, southwest, west, northwest)
  }

  // Alternative naming
  /*
  object HexRingAlt {
    val north: HexPoint = HexPoint(y = 1, z = -1)
    val northeast: HexPoint = HexPoint(x = 1, z = -1)
    val southeast: HexPoint = HexPoint(x = 1, y = -1)
    val south: HexPoint = HexPoint(y = -1, z = 1)
    val southwest: HexPoint = HexPoint(x = -1, z = 1)
    val northwest: HexPoint = HexPoint(x = -1, y = 1)

    val directions: Seq[HexPoint] = Seq(north, northeast, southeast, south, southwest, northwest)
  }
  */

}
