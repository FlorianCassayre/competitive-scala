package competitivescala.numbers

object Combinatorics {

  // n!
  def factorial[N](n: N)(implicit ev: Integral[N]): N = {
    import ev._
    require(n >= zero)
    def iterate(n: N, acc: N): N = if(n > one) iterate(n - one, acc * n) else acc
    iterate(n, one)
  }


  // upper! / lower!
  def partialFactorial[N](lower: N, upper: N)(implicit ev: Integral[N]): N = {
    import ev._
    require(lower >= zero)
    require(lower <= upper)
    def iterate(n: N, acc: N): N = if(n >= lower && n > one) iterate(n - one, acc * n) else acc
    iterate(upper, one)
  }


  // n choose k
  def combinations[N](n: N, k: N)(implicit ev: Integral[N]): N = {
    import ev._
    if(n < k || k < zero) { // require(k >= zero); require(n >= k)
      zero
    } else {
      val r = min(k, n - k)
      def iterate(i: N, acc: N): N = if(i < r) iterate(i + one, acc * (n - i) / (i + one)) else acc
      iterate(zero, one)
    }
  }


  // Number of permutations with repeating items
  def permutationsFromGroups[N](groups: Map[N, N])(implicit ev: Integral[N]): N = {
    import ev._
    if(groups.nonEmpty) {
      val total = groups.foldLeft(zero) { case (acc, (k, v)) => acc + k * v }
      def computeFactorials(n: N, acc: N, map: Map[N, N]): Map[N, N] = {
        val newMap = if(groups.contains(n) || n == total) map + (n -> acc) else map
        if(n < total) {
          val m = n + one
          computeFactorials(m, acc * m, newMap)
        } else {
          newMap
        }
      }
      def pow(a: N, b: N, acc: N): N = if(b > zero) pow(a, b - one, a * acc) else acc
      val factorials = computeFactorials(zero, one, Map.empty)
      val denominator = groups.foldLeft(one) { case (acc, (k, v)) => acc * pow(factorials(k), v, one) }
      factorials(total) / denominator
    } else {
      one
    }
  }

  def permutationsFromCounts[N](counts: Seq[N])(implicit ev: Integral[N]): N =
    permutationsFromGroups(counts.groupBy(identity).view.mapValues(vs => ev.fromInt(vs.size)).toMap)

  def permutations[T, N](objects: Seq[T])(implicit ev: Integral[N]): N = {
    permutationsFromCounts(objects.groupBy(identity).values.map(vs => ev.fromInt(vs.size)).toSeq)
  }

}
