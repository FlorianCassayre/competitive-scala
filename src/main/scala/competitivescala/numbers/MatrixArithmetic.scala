package competitivescala.numbers

object MatrixArithmetic {

  def dimensions[T](matrix: IndexedSeq[IndexedSeq[T]]): (Int, Int) = {
    require(matrix.nonEmpty && matrix.head.nonEmpty)
    val (m, n) = (matrix.size, matrix.head.size)
    require(matrix.forall(_.size == n))
    (m, n)
  }

  def elementwise[T](op: (T, T) => T)(matrix1: IndexedSeq[IndexedSeq[T]], matrix2: IndexedSeq[IndexedSeq[T]]): IndexedSeq[IndexedSeq[T]] = {
    val ((m1, n1), (m2, n2)) = (dimensions(matrix1), dimensions(matrix2))
    require(m1 == m2 && n1 == n2)
    IndexedSeq.tabulate(m1, n1)((i, j) => op(matrix1(i)(j), matrix2(i)(j)))
  }

  def multiply[N](matrix1: IndexedSeq[IndexedSeq[N]], matrix2: IndexedSeq[IndexedSeq[N]])(implicit ev: Numeric[N]): IndexedSeq[IndexedSeq[N]] = {
    import ev._
    val ((m1, n1), (m2, n2)) = (dimensions(matrix1), dimensions(matrix2))
    require(n1 == m2)
    IndexedSeq.tabulate(m1, n2)((i, j) => (0 until n1).foldLeft(zero)((acc, k) => acc + matrix1(i)(k) * matrix2(k)(j)))
  }

  def gaussianElimination[N](matrix: IndexedSeq[IndexedSeq[N]], precision: N)(implicit ev: Fractional[N]): IndexedSeq[IndexedSeq[N]] = {
    import ev._
    val (m, n) = dimensions(matrix)
    val (reducedMatrix, _) = (0 until m).foldLeft((matrix, -1)) { case ((matrix1, r), j) =>
      val k = (r + 1 until m).maxBy(i => abs(matrix1(i)(j)))
      val pivot = matrix1(k)(j)
      if(abs(pivot) > precision) {
        val r1 = r + 1
        val matrix2 = (0 until n).foldLeft(matrix1)((current, l) => current.updated(k, current(k).updated(l, current(k)(l) / pivot)))
        val matrix3 = (0 until n).foldLeft(matrix2)((current, l) => current.updated(k, current(k).updated(l, current(r1)(l))).updated(r1, current(r1).updated(l, current(k)(l))))
        val matrix4 = (0 until m).foldLeft(matrix3) { (current, i) =>
          val v = current(i)(j)
          if(i != r1) {
            (0 until n).foldLeft(current)((current, l) => current.updated(i, current(i).updated(l, current(i)(l) - current(r1)(l) * v)))
          } else {
            current
          }
        }
        (matrix4, r1)
      } else {
        (matrix1, r)
      }
    }
    reducedMatrix
  }

  def identity[N](n: Int)(implicit ev: Numeric[N]): IndexedSeq[IndexedSeq[N]] = IndexedSeq.tabulate(n, n)((i, j) => if(i == j) ev.one else ev.zero)

  def inverse[N](matrix: IndexedSeq[IndexedSeq[N]], precision: N)(implicit ev: Fractional[N]): Option[IndexedSeq[IndexedSeq[N]]] = {
    import ev._
    val (m, n) = dimensions(matrix)
    require(m == n)
    val matrices = IndexedSeq(matrix, identity(m))
    val result = gaussianElimination(IndexedSeq.tabulate(m, 2 * m)((i, j) => matrices(j / m)(i)(j % m)), precision)
    if(abs(result(m - 1)(m - 1) - one) <= precision) {
      Some(IndexedSeq.tabulate(m, m)((i, j) => result(i)(m + j)))
    } else {
      None
    }
  }

  def multiplyGroup[N](matrix1: IndexedSeq[IndexedSeq[N]], matrix2: IndexedSeq[IndexedSeq[N]], mod: N)(implicit ev: Integral[N]): IndexedSeq[IndexedSeq[N]] = {
    import ev._
    val ((m1, n1), (m2, n2)) = (dimensions(matrix1), dimensions(matrix2))
    require(n1 == m2)
    IndexedSeq.tabulate(m1, n2)((i, j) => (((0 until n1).foldLeft(zero)((acc, k) => acc + matrix1(i)(k) * matrix2(k)(j)) % mod) + mod) % mod)
  }

  def gaussianEliminationGroup[N](matrix: IndexedSeq[IndexedSeq[N]], mod: N)(implicit ev: Integral[N]): IndexedSeq[IndexedSeq[N]] = {
    import ev._
    def positiveMod(a: N): N = ((a % mod) + mod) % mod
    def modularInverse(a: N): N = {
      def iterate(tn: N, t: N, rn: N, r: N): N = {
        if (r != zero) {
          val q = rn / r
          iterate(t, tn - q * t, r, rn - q * r)
        } else {
          if (rn > one) throw new Exception else if (tn < zero) tn + mod else tn
        }
      }
      iterate(zero, one, mod, a)
    }
    val (m, n) = dimensions(matrix)
    val (reducedMatrix, _) = (0 until m).foldLeft((matrix.map(_.map(positiveMod)), -1)) { case ((matrix1, r), j) =>
      (r + 1 until m).find(i => matrix1(i)(j) != zero) match {
        case Some(k) =>
          val pivot = matrix1(k)(j)
          val pivotInverse = modularInverse(pivot)
          val r1 = r + 1
          val matrix2 = (0 until n).foldLeft(matrix1)((current, l) => current.updated(k, current(k).updated(l, (current(k)(l) * pivotInverse) % mod)))
          val matrix3 = (0 until n).foldLeft(matrix2)((current, l) => current.updated(k, current(k).updated(l, current(r1)(l))).updated(r1, current(r1).updated(l, current(k)(l))))
          val matrix4 = (0 until m).foldLeft(matrix3) { (current, i) =>
            val v = current(i)(j)
            if(i != r1) {
              (0 until n).foldLeft(current)((current, l) => current.updated(i, current(i).updated(l, positiveMod(current(i)(l) - current(r1)(l) * v))))
            } else {
              current
            }
          }
          (matrix4, r1)
        case None => (matrix1, r)
      }
    }
    reducedMatrix
  }

  def inverseGroup[N](matrix: IndexedSeq[IndexedSeq[N]], mod: N)(implicit ev: Integral[N]): Option[IndexedSeq[IndexedSeq[N]]] = {
    import ev._
    val (m, n) = dimensions(matrix)
    require(m == n)
    val matrices = IndexedSeq(matrix, identity(m))
    val result = gaussianEliminationGroup(IndexedSeq.tabulate(m, 2 * m)((i, j) => matrices(j / m)(i)(j % m)), mod)
    if(result(m - 1)(m - 1) == one) {
      Some(IndexedSeq.tabulate(m, m)((i, j) => result(i)(m + j)))
    } else {
      None
    }
  }


  def rotateClockwise[T](matrix: IndexedSeq[IndexedSeq[T]], i: Int = 1): IndexedSeq[IndexedSeq[T]] = ((i % 4) + 4) % 4 match {
    case 0 => matrix
    case 1 => matrix.transpose.map(_.reverse)
    case 2 => matrix.reverse.map(_.reverse)
    case 3 => matrix.transpose.reverse
  }

}
