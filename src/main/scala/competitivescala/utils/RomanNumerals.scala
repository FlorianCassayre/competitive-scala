package competitivescala.utils

object RomanNumerals {

  val I = 'I'
  val V = 'V'
  val X = 'X'
  val L = 'L'
  val C = 'C'
  val D = 'D'
  val M = 'M'

  // Note: does not check validity
  def parseRomanNumerals[N](string: String)(implicit ev: Integral[N]): N = {
    import ev._
    require(string.nonEmpty)
    val table = Map(
      I -> 1,
      V -> 5,
      X -> 10,
      L -> 50,
      C -> 100,
      D -> 500,
      M -> 1000,
    )
    def iterate(i: Int, acc: N): N = {
      if(i < string.length) {
        val value = ev.fromInt(table(string(i)))
        if(i < string.length - 1 && table(string(i)) < table(string(i + 1))) {
          iterate(i + 2, acc + (ev.fromInt(table(string(i + 1))) - value))
        } else {
          iterate(i + 1, acc + value)
        }
      } else {
        acc
      }
    }
    iterate(0, ev.zero)
  }

  def toRomanNumerals[N](n: N)(implicit ev: Integral[N]): String = {
    import ev._
    require(n > ev.zero)
    val table = IndexedSeq(
      1 -> s"$I",
      4 -> s"$I$V",
      5 -> s"$V",
      9 -> s"$I$X",
      10 -> s"$X",
      40 -> s"$X$L",
      50 -> s"$L",
      90 -> s"$X$C",
      100 -> s"$C",
      100 -> s"$C",
      400 -> s"$C$D",
      500 -> s"$D",
      900 -> s"$C$M",
      1000 -> s"$M",
    )
    def iterate(n: N, i: Int, acc: Seq[String]): String = {
      if(n > ev.zero) {
        val (valueInt, symbol) = table(i)
        val value = ev.fromInt(valueInt)
        iterate(n % value, i - 1, (symbol * ev.toInt(n / value)) +: acc)
      } else {
        acc.reverse.mkString
      }
    }
    iterate(n, table.size - 1, Seq.empty)
  }

}
