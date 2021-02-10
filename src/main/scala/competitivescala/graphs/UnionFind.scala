package competitivescala.graphs

object UnionFind {

  def find[U](x: U, forest: Map[U, U]): (U, Map[U, U]) = {
    forest.get(x) match {
      case Some(y) if x != y =>
        val (parent, newForest) = find(y, forest)
        (parent, newForest + (x -> parent))
      case _ => (x, forest)
    }
  }

  def union[U](x: U, y: U, forest: Map[U, U], ranks: Map[U, Int]): (Map[U, U], Map[U, Int]) = {
    val (xr, forest1) = find(x, forest)
    val (yr, forest2) = find(y, forest1)
    if(xr != yr) {
      val rx = ranks.getOrElse(xr, 0)
      val ry = ranks.getOrElse(yr, 0)
      if(rx < ry) {
        (forest2 + (xr -> yr), ranks)
      } else {
        val forest3 = forest2 + (yr -> xr)
        if(rx == ry) {
          (forest3, ranks + (xr -> (ranks.getOrElse(xr, 0) + 1)))
        } else {
          (forest3, ranks)
        }
      }
    } else {
      (forest2, ranks)
    }
  }

}
