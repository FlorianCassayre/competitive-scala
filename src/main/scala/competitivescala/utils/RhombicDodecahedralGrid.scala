package competitivescala.utils

object RhombicDodecahedralGrid {

  case class RDPoint(x: Int = 0, y: Int = 0, z: Int = 0, w: Int = 0) {
    def pointwise(f: (Int, Int) => Int)(that: RDPoint): RDPoint = RDPoint(f(x, that.x), f(y, that.y), f(z, that.z), f(w, that.w))
    def map(f: Int => Int): RDPoint = RDPoint(f(x), f(y), f(z), f(w))
    def +(that: RDPoint): RDPoint = pointwise(_ + _)(that)
    def -(that: RDPoint): RDPoint = pointwise(_ - _)(that)
    def unary_- : RDPoint = map(-_)
    def hexEuclidean: Int = (x.abs + y.abs + z.abs + w.abs) / 2
  }

  object RDRing {
    val top: RDPoint = RDPoint(x = -1, y = 1)
    val front: RDPoint = RDPoint(z = -1, w = 1)
    val topfrontleft: RDPoint = RDPoint(y = 1, z = -1)
    val topfrontright: RDPoint = RDPoint(x = -1, w = 1)
    val topbackleft: RDPoint = RDPoint(y = 1, w = -1)
    val topbackright: RDPoint = RDPoint(x = -1, z = 1)
    val bottom: RDPoint = -top
    val back: RDPoint = -front
    val bottombackright: RDPoint = -topfrontleft
    val bottombackleft: RDPoint = -topfrontright
    val bottomfrontright: RDPoint = -topbackleft
    val bottomfrontleft: RDPoint = -topbackright

    val all: Seq[RDPoint] = Seq(top, front, bottom, back, topfrontleft, topfrontright, topbackleft, topbackright, bottomfrontleft, bottomfrontright, bottombackleft, bottombackright)

    val vertices4: Seq[Seq[RDPoint]] = all.combinations(4).filter(_.reduce(_ + _).hexEuclidean == 4).toSeq
    val vertices3: Seq[Seq[RDPoint]] = all.combinations(3).filter { ps => val p = ps.reduce(_ + _); p.x * p.y * p.z * p.w == -3 }.toSeq
  }

}
