package algorithms.graphs.bipartite

import org.scalatest.funsuite.AnyFunSuite

import BipartiteMatchingUnweighted._

class BipartiteMatchingUnweightedTest extends AnyFunSuite {

  test("small perfect matching") {
    val adjacency = Map(0 -> Set("a", "b"), 1 -> Set("b", "c"), 2 -> Set("a"))
    assert(maxMatching(adjacency) == Map(0 -> "b", 1 -> "c", 2 -> "a"))
  }

  test("small partial matching") {
    val adjacency = Map(0 -> Set("a"), 1 -> Set.empty[String], 2 -> Set("c"))
    assert(maxMatching(adjacency) == Map(0 -> "a", 2 -> "c"))
  }

  test("small multiple matching") {
    val adjacency = Map(0 -> Set("a", "b"), 1 -> Set("a", "b"), 2 -> Set("a", "b", "c"))
    val matching = maxMatching(adjacency)
    assert(matching == Map(0 -> "a", 1 -> "b", 2 -> "c") || matching == Map(0 -> "b", 1 -> "a", 2 -> "c"))
  }

}
