package competitivescala.geometry

object PointPolygon {

  def pnpoly[N](point: (N, N), polygon: Seq[(N, N)])(implicit ev: Fractional[N]): Boolean = {
    import ev._
    val (x, y) = point
    polygon.zip(polygon.last +: polygon).foldLeft(false) { case (b, ((ix, iy), (jx, jy))) =>
      ((iy > y != jy > y) && (x < (jx - ix) * (y - iy) / (jy - iy) + ix)) ^ b
    }
  }

}
