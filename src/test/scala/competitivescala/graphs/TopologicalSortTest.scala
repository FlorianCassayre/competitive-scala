package competitivescala.graphs

import TopologicalSort._
import org.scalatest.funsuite.AnyFunSuite

class TopologicalSortTest extends AnyFunSuite {

  test("topological sort") {
    assert(
      topologicalSort(Map('a' -> Set('b', 'c'), 'b' -> Set('c'), 'c' -> Set.empty[Char])) ==
        Seq('a', 'b', 'c')
    )

    assert(
      topologicalSort(Map('a' -> Set('d', 'e'), 'b' -> Set('a', 'd'), 'c' -> Set('e'), 'd' -> Set('c'), 'e' -> Set.empty[Char])) ==
        Seq('b', 'a', 'd', 'c', 'e')
    )
  }

}
