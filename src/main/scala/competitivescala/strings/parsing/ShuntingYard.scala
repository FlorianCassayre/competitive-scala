package competitivescala.strings.parsing

object ShuntingYard {

  sealed abstract class Expression {
    def value: Int = this match {
      case Literal(v) => v
      case Addition(a, b) => a.value + b.value
      case Multiplication(a, b) => a.value * b.value
    }
  }
  case class Literal(v: Int) extends Expression
  sealed abstract class Operation(a: Expression, b: Expression) extends Expression
  case class Addition(a: Expression, b: Expression) extends Operation(a, b)
  case class Multiplication(a: Expression, b: Expression) extends Operation(a, b)

  def parseExpression(tokens: Seq[String]): Expression = {
    val precedences = Seq(Map("+" -> Addition), Map("*" -> Multiplication))
    val operators = precedences.flatten.toMap
    def precedence(op: String): Int = precedences.indexWhere(_.contains(op))
    def reduce(values: Seq[Expression], operations: Seq[String]): Seq[Expression] =
      operations.foldLeft(values)((acc, op) => operators(op)(acc(1), acc(0)) +: acc.drop(2))
    def parse(tokens: Seq[String]): Expression = {
      val (output, stack, _) = tokens.foldLeft((Seq.empty[Expression], Seq(Seq.empty[String]), false)) { case ((output, stack, last), token) =>
        (token, last) match {
          case (lit, false) if lit.forall(_.isDigit) => (Literal(lit.toInt) +: output, stack, true)
          case ("(", false) => (output, Seq.empty +: stack, false)
          case (")", true) =>
            assert(stack.tail.nonEmpty)
            (reduce(output, stack.head), stack.tail, true)
          case (op, true) if operators.contains(op) =>
            val (left, right) = stack.head.span(precedence(_) >= precedence(op))
            (reduce(output, left), (op +: right) +: stack.tail, false)
          case _ => throw new Exception
        }
      }
      assert(stack.tail.isEmpty)
      val result = stack.headOption.map(reduce(output, _)).getOrElse(output)
      assert(result.tail.isEmpty)
      result.head
    }
    parse(tokens)
  }

  def tokenizeExpression(expression: String): Seq[String] = expression.split("\\s+").flatMap(_.foldLeft((Seq.empty[String], false)) {
    case ((acc, lastDigit), c) =>
      val isDigit = c.isDigit
      if (lastDigit && isDigit) ((acc.head + c) +: acc.tail, true) else (c.toString +: acc, isDigit)
  }._1.reverse)

}
