package competitivescala.graphs

object ShortestPath {

  // Complexity: O((E + V) * log V)
  // Weights must be positive
  def shortestPathDijkstra[U, N](adjacency: Map[U, Set[(U, N)]], source: U, earlyStopping: Set[U] = Set.empty[U])(implicit ev: Numeric[N]): (Map[U, N], Map[U, U]) = {
    import ev._
    import scala.collection.immutable.TreeSet
    def search(queue: TreeSet[(U, N)], distances: Map[U, N], predecessors: Map[U, U], visited: Set[U]): (Map[U, N], Map[U, U]) = {
      queue.headOption match {
        case Some((u, du)) =>
          if(earlyStopping.contains(u)) {
            (distances, predecessors)
          } else if(!visited.contains(u)) {
            val newVisited = visited + u
            val edges = adjacency.getOrElse(u, Set.empty).filter { case (b, w) => !visited.contains(b) && (!distances.contains(b) || distances(b) > du + w) }
            val (newQueue, newDistances, newPredecessors) = edges.foldLeft((queue, distances, predecessors)) { case ((currentQueue, currentDistances, currentPredecessors), (v, w)) =>
              val newDistance = du + w
              (currentDistances.get(v).map(dv => currentQueue - ((v, dv))).getOrElse(currentQueue) + ((v, newDistance)),
                currentDistances - v + (v -> newDistance),
                currentPredecessors + (v -> u))
            }
            search(newQueue, newDistances, newPredecessors, newVisited)
          } else {
            search(queue.tail, distances, predecessors, visited)
          }
        case None => (distances, predecessors)
      }
    }
    search(TreeSet((source, zero))(Ordering.by(_._2)), Map(source -> zero), Map.empty, Set.empty)
  }


  // Complexity: O(V * E)
  // Negative weights are allowed, reachable negative cycles will be detected and distances will be propagated accordingly
  def shortestPathBellmanFord[U, N](adjacency: Map[U, Set[(U, N)]], source: U)(implicit ev: Numeric[N]): (Map[U, Option[N]], Map[U, U]) = {
    import ev._
    val edges = adjacency.flatMap { case (u, set) => set.map { case (v, w) => ((u, v), w) } }
    def iterate(i: Int, distances: Map[U, N], predecessors: Map[U, U]): (Map[U, N], Map[U, U]) = {
      if(i > 0) {
        val (newDistances, newPredecessors) = edges.foldLeft((distances, predecessors)) { case ((currentDistances, currentPredecessors), ((u, v), w)) =>
          currentDistances.get(u) match {
            case Some(du) =>
              val newDistance = du + w
              currentDistances.get(v) match {
                case Some(dv) if dv <= newDistance => (currentDistances, currentPredecessors)
                case _ => (currentDistances + (v -> newDistance), currentPredecessors + (v -> u))
              }
            case None => (currentDistances, currentPredecessors)
          }
        }
        iterate(i - 1, newDistances, newPredecessors)
      } else {
        (distances, predecessors)
      }
    }
    val (distances, predecessors) = iterate(adjacency.size, Map(source -> zero), Map.empty)
    val cycleSources = edges.collect { case ((u, v), w) if distances.contains(u) && distances(u) + w < distances(v) => v }.toSet
    def bfs(vertices: Set[U], visited: Set[U]): Set[U] = {
      if(vertices.nonEmpty) {
        val next = vertices.flatMap(adjacency.getOrElse(_, Set.empty).map(_._1)).diff(visited)
        bfs(next, visited ++ next)
      } else {
        visited
      }
    }
    val cycles = bfs(cycleSources, cycleSources)
    val distancesCycles = distances.map { case (u, d) => u -> (if(cycles.contains(u)) None else Some(d)) }
    (distancesCycles, predecessors)
  }


  def reconstructPath[U](target: U, predecessors: Map[U, U]): Seq[U] = {
    def reconstruct(v: U, acc: Seq[U]): Seq[U] = {
      val newAcc = v +: acc
      predecessors.get(v) match {
        case Some(u) => reconstruct(u, newAcc)
        case None => newAcc
      }
    }
    reconstruct(target, Seq.empty)
  }

}
