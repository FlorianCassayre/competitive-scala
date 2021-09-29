package competitivescala.graphs

import TopologicalSort._
import org.scalatest.funsuite.AnyFunSuite

class TopologicalSortTest extends AnyFunSuite {

  test("topological sort on small graphs") {
    assert(
      topologicalSort(Map('a' -> Set('b', 'c'), 'b' -> Set('c'), 'c' -> Set.empty[Char])) ==
        Seq('a', 'b', 'c')
    )

    assert(
      topologicalSort(Map('a' -> Set('d', 'e'), 'b' -> Set('a', 'd'), 'c' -> Set('e'), 'd' -> Set('c'), 'e' -> Set.empty[Char])) ==
        Seq('b', 'a', 'd', 'c', 'e')
    )
  }

  test("topological sort on an extreme deep and shallow graph") {
    val nodes = (0 until 10000).map(_.toString)
    val graph = nodes.zip(nodes.tail).map { case (a, b) => a -> Set(b) }.toMap

    assert(topologicalSort(graph) == nodes)
  }

  test("topological sort on a singleton graph") {
    assert(topologicalSort(Map('a' -> Set('b'))) == Seq('a', 'b'))
  }

  test("topological sort on an empty graph") {
    assert(topologicalSort(Map.empty) == Seq.empty)
  }

  test("topological sort on a cyclic graph (illegal)") {
    assertThrows[Exception](topologicalSort(Map('a' -> Set('b'), 'b' -> Set('c'), 'c' -> Set('a'))))
  }

}
