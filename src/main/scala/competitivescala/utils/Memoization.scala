package competitivescala.utils

object Memoization {

  case class Memoizer[K, V, A](run: Map[K, V] => (Map[K, V], A)) {
    def apply(map: Map[K, V] = Map.empty): (Map[K, V], A) = run(map)
    def evalState(s: Map[K, V]): A = run(s)._2
    def execState(s: Map[K, V]): Map[K, V] = run(s)._1
    def map[B](ab: A => B): Memoizer[K, V, B] =
      Memoizer(s => run(s) match {
        case (s, a) => (s, ab(a))
      })
    def flatMap[B](afb: A => Memoizer[K, V, B]): Memoizer[K, V, B] =
      Memoizer(s => run(s) match {
        case (s, a) => afb(a).run(s)
      })
  }
  object Memoizer {
    def point[K, V, A](a: A): Memoizer[K, V, A] = Memoizer((_, a))
    def get[K, V]: Memoizer[K, V, Map[K, V]] = Memoizer(s => (s, s))
    def gets[K, V, A](f: Map[K, V] => A): Memoizer[K, V, A] = Memoizer(s => (s, f(s)))
    def put[K, V](s: Map[K, V]): Memoizer[K, V, Unit] = Memoizer(_ => (s, ()))
    def modify[K, V](ss: Map[K, V] => Map[K, V]): Memoizer[K, V, Unit] = for { s <- get; _ <- put(ss(s)) } yield ()
  }

  def memoized[K, V](f: (V => Memoizer[K, V, V], K => Memoizer[K, V, V]) => K => Memoizer[K, V, V]): K => Memoizer[K, V, V] = input => for {
    cachedOpt <- Memoizer.gets[K, V, Option[V]](_.get(input))
    cachedResult <- cachedOpt match {
      case Some(cached) => Memoizer.point[K, V, V](cached)
      case None => for {
        result <- f(Memoizer.point[K, V, V], memoized(f))(input)
        _ <- Memoizer.modify[K, V](_ + (input -> result))
      } yield result
    }
  } yield cachedResult

  def memoizedEval[K, V](f: (V => Memoizer[K, V, V], K => Memoizer[K, V, V]) => K => Memoizer[K, V, V]): K => V = input => memoized(f)(input)()._2

}
