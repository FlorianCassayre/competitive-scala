package competitivescala.graphs.bipartite

object BipartiteMatchingUnweighted {

  def maxMatching[U, V](adjacency: Map[U, Set[V]]): Map[U, V] = {
    val (us, vs) = (adjacency.keySet.toSeq, adjacency.values.toSeq.flatten.distinct)
    def maxMatching(remainingUs: Seq[U], matchedVs: Map[V, U]): Map[V, U] = {
      def findMatch(u: U, seenVs: Set[V], matchedVs: Map[V, U]): Option[(Set[V], Map[V, U])] = {
        def iterate(remainingVs: Seq[V], seenVs: Set[V], matchedVs: Map[V, U]): Option[(Set[V], Map[V, U])] = {
          remainingVs match {
            case v +: tail =>
              if (adjacency(u).contains(v) && !seenVs.contains(v)) {
                val newSeen = seenVs + v
                if (!matchedVs.contains(v)) {
                  Some(newSeen, matchedVs + (v -> u))
                } else {
                  findMatch(matchedVs(v), newSeen, matchedVs) match {
                    case Some((newSeenVs, newMatched)) => Some(newSeenVs, newMatched + (v -> u))
                    case None => iterate(tail, newSeen, matchedVs)
                  }
                }
              } else {
                iterate(tail, seenVs, matchedVs)
              }
            case _ => None
          }
        }
        iterate(vs, seenVs, matchedVs)
      }
      remainingUs match {
        case u +: tail =>
          val newMatchedVs = findMatch(u, Set.empty, matchedVs).map(_._2).getOrElse(matchedVs)
          maxMatching(tail, newMatchedVs)
        case _ => matchedVs
      }
    }
    maxMatching(us, Map.empty).map(_.swap)
  }

}
