package competitivescala.strings

object RunLength {

  def runLengthEncode[T](seq: Seq[T]): Seq[(T, Int)] = {
    val result = seq.headOption match {
      case Some(first) =>
        seq.tail.foldLeft(Seq((first, 1))) { case (result@((head, count) +: tail), e) =>
          if (e == head) {
            (head, count + 1) +: tail
          } else {
            (e, 1) +: result
          }
        }
      case None => Seq.empty
    }
    result.reverse
  }

  def runLengthDecode[T](seq: Seq[(T, Int)]): Seq[T] =
    seq.flatMap { case (e, count) => Seq.fill(count)(e) }

}
