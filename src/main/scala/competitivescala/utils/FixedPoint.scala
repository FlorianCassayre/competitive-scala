package competitivescala.utils

object FixedPoint {

  def applyRecursiveMemoizedState[S, T, N](f: S => S, g: S => T)(initial: S, n: N)(implicit ev: Integral[N]): S = {
    import ev._
    require(n >= zero)
    def recurse(s: S, k: Int, history: IndexedSeq[S], indexedHistory: Map[T, Int]): S = {
      if(ev.fromInt(k) < n) {
        val s1 = f(s)
        val t1 = g(s1)
        val k1 = k + 1
        indexedHistory.get(t1) match {
          case Some(i) =>
            val period = k1 - i
            val j = ((n - ev.fromInt(k1)) % ev.fromInt(period)).toInt
            history(i + j)
          case None =>
            recurse(s1, k1, history :+ s1, indexedHistory + (t1 -> k1))
        }
      } else {
        s
      }
    }
    recurse(initial, 0, IndexedSeq(initial), Map(g(initial) -> 0))
  }

  // A simpler version of the above
  def applyRecursiveMemoized[T, N](f: T => T)(initial: T, n: N)(implicit ev: Integral[N]): T =
    applyRecursiveMemoizedState(f, identity[T])(initial, n)

}
