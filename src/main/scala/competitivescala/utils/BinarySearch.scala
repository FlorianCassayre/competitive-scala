package competitivescala.utils

object BinarySearch {

  def binarySearch[T](array: IndexedSeq[T], needle: T)(implicit ord: Ordering[T]): Option[Int] = {
    import ord._
    def find(start: Int, end: Int): Option[Int] = {
      if(start <= end) {
        val mid = (start + end) / 2
        val value = array(mid)
        if(value == needle) {
          Some(mid)
        } else {
          if(value < needle) {
            find(mid + 1, end)
          } else {
            find(start, mid - 1)
          }
        }
      } else {
        None
      }
    }
    find(0, array.size - 1)
  }

  // ... X N N Y ... with needle = N
  //     ^_ _ _ ____ upper = f, strict = t (greatest element less than needle)
  //       ^_ _ ____ upper = f, strict = f (first occurrence of needle)
  //         ^_ ____ upper = t, strict = f (last occurrence of needle)
  //           ^____ upper = t, strict = t (least element greater than needle)
  def binarySearchBound[T](array: IndexedSeq[T], needle: T, upper: Boolean, strict: Boolean = false)(implicit ord: Ordering[T]): Option[Int] = {
    def find(start: Int, end: Int, found: Option[Int]): Option[Int] = {
      if(start <= end) {
        val mid = (start + end) / 2
        val value = array(mid)
        if(upper) {
          val cmp = ord.compare(value, needle)
          if(cmp < 0 || !strict && cmp == 0) {
            find(mid + 1, end, Some(mid))
          } else {
            find(start, mid - 1, found)
          }
        } else {
          val cmp = ord.compare(value, needle)
          if(cmp > 0 || !strict && cmp == 0) {
            find(start, mid - 1, Some(mid))
          } else {
            find(mid + 1, end, found)
          }
        }
      } else {
        found
      }
    }
    find(0, array.size - 1, None)
  }

}
