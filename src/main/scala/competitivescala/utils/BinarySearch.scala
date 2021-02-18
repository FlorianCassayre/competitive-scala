package competitivescala.utils

object BinarySearch {

  def binarySearch[N, T](mapping: N => T, first: N, last: N, needle: T)(implicit ev: Integral[N], ord: Ordering[T]): Option[N] = {
    import ev._
    val two = one + one
    def find(start: N, end: N): Option[N] = {
      if(start <= end) {
        val mid = (start + end) / two
        val value = mapping(mid)
        if(value == needle) {
          Some(mid)
        } else {
          if(ord.lt(value, needle)) {
            find(mid + one, end)
          } else {
            find(start, mid - one)
          }
        }
      } else {
        None
      }
    }
    find(first, last)
  }

  def binarySearchArray[T](array: IndexedSeq[T], lower: Int, upper: Int, needle: T)(implicit ord: Ordering[T]): Option[Int] =
    binarySearch(array, lower, upper, needle)

  def binarySearchArray[T](array: IndexedSeq[T], needle: T)(implicit ord: Ordering[T]): Option[Int] =
    binarySearchArray(array, 0, array.size - 1, needle)


  // ... X N N Y ... with needle = N
  //     ^_ _ _ ____ upper = f, strict = t (greatest element less than needle)
  //       ^_ _ ____ upper = f, strict = f (first occurrence of needle)
  //         ^_ ____ upper = t, strict = f (last occurrence of needle)
  //           ^____ upper = t, strict = t (least element greater than needle)
  def binarySearchBound[N, T](mapping: N => T, first: N, last: N, needle: T, upper: Boolean, strict: Boolean = false)(implicit ev: Integral[N], ord: Ordering[T]): Option[N] = {
    import ev._
    val two = one + one
    def find(start: N, end: N, found: Option[N]): Option[N] = {
      if(start <= end) {
        val mid = (start + end) / two
        val value = mapping(mid)
        if(upper) {
          val cmp = ord.compare(value, needle)
          if(cmp < 0 || !strict && cmp == 0) {
            find(mid + one, end, Some(mid))
          } else {
            find(start, mid - one, found)
          }
        } else {
          val cmp = ord.compare(value, needle)
          if(cmp > 0 || !strict && cmp == 0) {
            find(start, mid - one, Some(mid))
          } else {
            find(mid + one, end, found)
          }
        }
      } else {
        found
      }
    }
    find(first, last, None)
  }

  def binarySearchBoundArray[T](array: IndexedSeq[T], first: Int, last: Int, needle: T, upper: Boolean, strict: Boolean)(implicit ord: Ordering[T]): Option[Int] =
    binarySearchBound(array, first, last, needle, upper, strict)

  def binarySearchBoundArray[T](array: IndexedSeq[T], needle: T, upper: Boolean, strict: Boolean = false)(implicit ord: Ordering[T]): Option[Int] =
    binarySearchBoundArray(array, 0, array.size - 1, needle, upper, strict)

}
