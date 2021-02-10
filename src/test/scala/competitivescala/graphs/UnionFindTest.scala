package competitivescala.graphs

import UnionFind._
import org.scalatest.funsuite.AnyFunSuite

class UnionFindTest extends AnyFunSuite {

  def actionUnion[B](x: Char, y: Char): Left[(Char, Char), B] = Left(x, y)
  def actionFindExpect[A](x: Char, expectedY: Char): Right[A, (Char, Char)] = Right(x, expectedY)

  test("union find") {
    val operations = Seq(
      actionFindExpect('a', 'a'),
      actionUnion('a', 'b'),
      actionFindExpect('a', 'a'),
      actionFindExpect('b', 'a'),
      actionUnion('c', 'd'),
      actionFindExpect('d', 'c'),
      actionUnion('a', 'c'),
      actionFindExpect('d', 'a'),
      actionFindExpect('c', 'a'),
      actionFindExpect('b', 'a'),
      actionUnion('b', 'e'),
      actionFindExpect('e', 'a'),
    )

    operations.foldLeft((Map.empty[Char, Char], Map.empty[Char, Int])) { case ((forest, ranks), action) =>
      action match {
        case Left((x, y)) => union(x, y, forest, ranks)
        case Right((x, expectedY)) =>
          val (y, newForest) = find(x, forest)
          assert(y == expectedY)
          (newForest, ranks)
      }
    }
  }

}
