package competitivescala.geometry

import PointPolygon._

import org.scalatest.funsuite.AnyFunSuite

class PointPolygonTest extends AnyFunSuite {

  test("point in polygon") {
    val triangle = Seq((1.0, 1.0), (1.0, 2.0), (2.0, 1.0))
    assert(pnpoly((1.3, 1.2), triangle))
    assert(!pnpoly((0.5, 1.5), triangle))
    assert(!pnpoly((1.6, 1.6), triangle))
  }

}
