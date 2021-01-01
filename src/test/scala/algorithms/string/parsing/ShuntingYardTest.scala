package algorithms.string.parsing

import org.scalatest.funsuite.AnyFunSuite
import ShuntingYard._

import scala.util.Try

class ShuntingYardTest extends AnyFunSuite {

  test("correct tokenization") {
    assert(tokenizeExpression("1+(2*3)") == Seq("1", "+", "(", "2", "*", "3", ")"))
    assert(tokenizeExpression(" 1 + 23 * (45+ 67)+8   +9  ") == Seq(
      "1", "+", "23", "*", "(", "45", "+", "67", ")", "+", "8", "+", "9"
    ))
  }

  private def parse(expression: String): Expression = parseExpression(tokenizeExpression(expression))

  test("correct parsing") {
    assert(parse("1") == Literal(1))
    assert(parse("((1))") == Literal(1))
    assert(parse("(((1)+((2))))") == Addition(Literal(1), Literal(2)))
    assert(parse("1 + 2 + 3") == Addition(Addition(Literal(1), Literal(2)), Literal(3)))
    assert(parse("1 * 2 + 3") == Addition(Multiplication(Literal(1), Literal(2)), Literal(3)))
    assert(parse("(1 + 2 * 3)") == Addition(Literal(1), Multiplication(Literal(2), Literal(3))))
    assert(parse("1 + (2 + 3) * 4") == Addition(Literal(1), Multiplication(Addition(Literal(2), Literal(3)), Literal(4))))
  }

  test("correct reduction") {
    assert(parse("4+(8*13+(7+(2*4+5)*3))*(4+5)").value == 1354)
  }

  test("invalid expression") {
    Seq(
      "", "(", ")", "+", "1+", "+1", "()", ")(", ")1", "1(", "(1", "1)", ")1(", "1()", "()1", "+1 2", "1 2+", "1 2",
      "(1)2", "1(2)", "(1)(2)", "1+*2", "1(+)2", "(1+)2", "1(+2)", "(1+2", "1+2)", "((1+2)", "(1+2))", "1()+2", "1+()2",
      "1+(2)3", "1)(+1"
    ).foreach(expression => assert(Try(parse(expression)).isFailure, expression))
  }
}
