package competitivescala.graphs

object TopologicalSort {

  // Complexity: O(E + V)
  // Also bounded by call stack for extreme graphs!
  def topologicalSort[U](adjacency: Map[U, Set[U]]): Seq[U] = {
    def dfs(u: U, marks: Map[U, Boolean], remaining: Set[U], sorted: Seq[U]): (Map[U, Boolean], Set[U], Seq[U]) = {
      marks.get(u) match {
        case Some(true) => (marks, remaining, sorted)
        case Some(false) => throw new Exception // Cycle
        case None =>
          val (newMarks, newRemaining, newSorted) = adjacency.getOrElse(u, Set.empty[U]).foldLeft((marks + (u -> false), remaining - u, sorted)) {
            case ((marks, remaining, sorted), v) => dfs(v, marks, remaining, sorted)
          }
          (newMarks + (u -> true), newRemaining, u +: newSorted)
      }
    }
    def iterate(marks: Map[U, Boolean], remaining: Set[U], sorted: Seq[U]): Seq[U] = {
      if(remaining.nonEmpty) {
        val (newMarks, newRemaining, newSorted) = dfs(remaining.head, marks, remaining, sorted)
        iterate(newMarks, newRemaining, newSorted)
      } else {
        sorted
      }
    }
    iterate(Map.empty, adjacency.keySet ++ adjacency.values.flatten, Seq.empty)
  }

}
