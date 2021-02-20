package competitivescala.graphs

object UnionFind {

  type Internal[U] = (Map[U, U], Map[U, Int])
  case class UnionFindState[U, A](run: Internal[U] => (Internal[U], A)) {
    def map[B](ab: A => B): UnionFindState[U, B] =
      UnionFindState(s => run(s) match {
        case (s, a) => (s, ab(a))
      })
    def flatMap[B](afb: A => UnionFindState[U, B]): UnionFindState[U, B] =
      UnionFindState(s => run(s) match {
        case (s, a) => afb(a).run(s)
      })
  }
  object UnionFindState {
    def get[U]: UnionFindState[U, Internal[U]] = UnionFindState(s => (s, s))
    def modify[U](ss: Internal[U] => Internal[U]): UnionFindState[U, Unit] = for { s <- get; _ <- UnionFindState[U, Unit](_ => (ss(s), ())) } yield ()
  }

  def InitialInternal[U]: Internal[U] = (Map.empty, Map.empty)

  def find[U](x: U): UnionFindState[U, U] = for {
    state <- UnionFindState.get[U]
    (forest, _) = state
    found <- forest.get(x) match {
      case Some(y) if x != y => for {
        parent <- find(y)
        _ <- UnionFindState.modify[U] { case (forest, ranks) => (forest + (x -> parent), ranks) }
      } yield parent
      case _ => UnionFindState[U, U]((_, x))
    }
  } yield found

  def union[U](x: U, y: U): UnionFindState[U, Unit] = for {
    xr <- find(x)
    yr <- find(y)
    _ <- UnionFindState.modify[U] { case (forest, ranks) =>
      if(xr != yr) {
        val (rx, ry) = (ranks.getOrElse(xr, 0), ranks.getOrElse(yr, 0))
        if(rx < ry) {
          (forest + (xr -> yr), ranks)
        } else {
          val newForest = forest + (yr -> xr)
          if(rx == ry) {
            (newForest, ranks + (xr -> (ranks.getOrElse(xr, 0) + 1)))
          } else {
            (newForest, ranks)
          }
        }
      } else {
        (forest, ranks)
      }
    }
  } yield ()

}
