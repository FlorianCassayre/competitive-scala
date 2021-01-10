package competitivescala.graphs

object MinimumSpanningTree {

  // Complexity: O(E log E)
  // Kruskal's algorithm
  def minimumSpanningTree[U, N](graph: Map[(U, U), N])(implicit ev: Numeric[N]): (N, Set[(U, U)]) = {
    import ev._
    val (tree, weight, _) = graph.toSeq.sortBy(_._2).foldLeft((Set.empty[(U, U)], ev.zero, Set.empty[U])) {
      case ((tree, weight, seen), ((u, v), w)) =>
        if (!seen.contains(u) || !seen.contains(v))
          (tree + ((u, v)), weight + w, seen + u + v)
        else
          (tree, weight, seen)
    }
    (weight, tree)
  }

}
