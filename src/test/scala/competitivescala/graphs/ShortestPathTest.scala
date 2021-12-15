package competitivescala.graphs

import ShortestPath._

import org.scalatest.funsuite.AnyFunSuite

class ShortestPathTest extends AnyFunSuite {

  test("dijkstra all vertices") {
    assert(shortestPathDijkstra(Map('a' -> Set(('b', 1)), 'b' -> Set(('a', 2))), 'a') == (Map('a' -> 0, 'b' -> 1), Map('b' -> 'a')))
    assert(shortestPathDijkstra(Map('a' -> Set(('b', 1), ('c', 4)), 'b' -> Set(('c', 2), ('d', 4)), 'c' -> Set(('d', 1))).withDefaultValue(Set.empty), 'a') ==
      (Map('a' -> 0, 'b' -> 1, 'c' -> 3, 'd' -> 4), Map('b' -> 'a', 'c' -> 'b', 'd' -> 'c')))
    assert(shortestPathDijkstra(Map('s' -> Set(('t', 10), ('y', 5)), 't' -> Set(('x', 1), ('y', 2)), 'y' -> Set(('t', 3), ('x', 9), ('z', 2)), 'x' -> Set(('z', 4)), 'z' -> Set(('x', 6), ('s', 7))), 's') ==
      (Map('s' -> 0, 't' -> 8, 'y' -> 5, 'x' -> 9, 'z' -> 7), Map('t' -> 'y', 'y' -> 's', 'z' -> 'y', 'x' -> 't')))
    assert(shortestPathDijkstra(Map('a' -> Set(('b', 1), ('c', 1)), 'b' -> Set(('d', 1)), 'c' -> Set(('e', 1))).withDefaultValue(Set.empty), 'a') ==
      (Map('a' -> 0, 'b' -> 1, 'c' -> 1, 'd' -> 2, 'e' -> 2), Map('b' -> 'a', 'c' -> 'a', 'd' -> 'b', 'e' -> 'c')))
  }

  test("dijkstra early stopping") {
    // Notice that d(a, d) is normally 5
    assert(shortestPathDijkstra(Map('a' -> Set(('c', 4), ('b', 2), ('d', 7)), 'c' -> Set(('d', 2)), 'b' -> Set(('c', 1))), 'a', Set('c')) ==
      (Map('a' -> 0, 'b' -> 2, 'c' -> 3, 'd' -> 7), Map('b' -> 'a', 'c' -> 'b', 'd' -> 'a')))
  }

  test("bellman-ford") {
    assert(shortestPathBellmanFord(Map('a' -> Set(('b', 2), ('c', 1), ('d', 4)), 'b' -> Set(('c', 1), ('d', 1)), 'c' -> Set(('b', 2), ('d', 3))), 'a') ==
      (Map('a' -> Some(0), 'b' -> Some(2), 'c' -> Some(1), 'd' -> Some(3)), Map('b' -> 'a', 'c' -> 'a', 'd' -> 'b')))
    assert(shortestPathBellmanFord(Map('a' -> Set(('b', 1), ('c', 2)), 'c' -> Set(('d', 2)), 'd' -> Set(('e', 1)), 'e' -> Set(('c', -4), ('f', 6))), 'a') ==
      (Map('a' -> Some(0), 'b' -> Some(1), 'c' -> None, 'd' -> None, 'e' -> None, 'f' -> None), Map('b' -> 'a', 'c' -> 'e', 'd' -> 'c', 'e' -> 'd', 'f' -> 'e')))
  }

  test("path reconstruction") {
    assert(reconstructPath('a', Map.empty) == Seq('a'))
    assert(reconstructPath('d', Map('b' -> 'a', 'c' -> 'b', 'd' -> 'c', 'e' -> 'a', 'f' -> 'd')) == Seq('a', 'b', 'c', 'd'))
  }

}
