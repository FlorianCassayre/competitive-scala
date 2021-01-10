package competitivescala.graphs

import org.scalatest.funsuite.AnyFunSuite

import MinimumSpanningTree._

class MinimumSpanningTreeTest extends AnyFunSuite {

  test("minimum spanning tree") {
    assert(
      minimumSpanningTree(Map(('a', 'b') -> 4, ('a', 'c') -> 2, ('b', 'c') -> 3, ('d', 'a') -> 5)) ==
        (10, Set(('a', 'c'), ('b', 'c'), ('d', 'a')))
    )

    assert(minimumSpanningTree(Map.empty[(Char, Char), Int]) == (0, Set.empty[Int]))
    assert(minimumSpanningTree(Map(('a', 'b') -> 1)) == (1, Set(('a', 'b'))))
  }

}
