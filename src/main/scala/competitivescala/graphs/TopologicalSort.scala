package competitivescala.graphs

object TopologicalSort {

  // Complexity: O(E + V)
  def topologicalSort[U](adjacency: Map[U, Set[U]]): Seq[U] = {
    def dfs(stack: Seq[(U, Set[U])], marks: Map[U, Boolean], remaining: Set[U], sorted: Seq[U]): (Map[U, Boolean], Set[U], Seq[U]) = {
      stack match {
        case (u, adjacent) +: tail =>
          adjacent.headOption match {
            case Some(v) =>
              marks.get(v) match {
                case Some(false) => throw new Exception // Cycle
                case Some(true) => dfs((u, adjacent.tail) +: tail, marks, remaining, sorted)
                case None => dfs((v, adjacency.getOrElse(v, Set.empty[U])) +: (u, adjacent.tail) +: tail, marks + (v -> false), remaining, sorted)
              }
            case None => dfs(tail, marks + (u -> true), remaining - u, u +: sorted)
          }
        case _ => (marks, remaining, sorted)
      }
    }
    def iterate(marks: Map[U, Boolean], remaining: Set[U], sorted: Seq[U]): Seq[U] = {
      if(remaining.nonEmpty) {
        val u = remaining.head
        val (newMarks, newRemaining, newSorted) = dfs(Seq((u, adjacency.getOrElse(u, Set.empty[U]))), marks + (u -> false), remaining - u, sorted)
        iterate(newMarks, newRemaining, newSorted)
      } else {
        sorted
      }
    }
    iterate(Map.empty, adjacency.keySet ++ adjacency.values.flatten, Seq.empty)
  }

}
