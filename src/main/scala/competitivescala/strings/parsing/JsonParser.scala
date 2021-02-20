package competitivescala.strings.parsing

object JsonParser {

  // Simple monadic recursive parser

  sealed abstract class JsonValue
  sealed abstract class JsonNested extends JsonValue
  case class JsonObject(obj: Map[String, JsonValue]) extends JsonNested
  case class JsonArray(array: IndexedSeq[JsonValue]) extends JsonNested
  sealed abstract class JsonPrimitive extends JsonValue
  case class JsonString(string: String) extends JsonPrimitive
  case class JsonNumber(number: String) extends JsonPrimitive
  case class JsonBoolean(boolean: Boolean) extends JsonPrimitive
  case object JsonNull extends JsonPrimitive

  def parseJson(string: String): JsonNested = {
    case class Parser[+A](run: Int => Option[(Int, A)]) {
      def map[B](ab: A => B): Parser[B] =
        Parser(s => run(s).map {
          case (s, a) => (s, ab(a))
        })
      def flatMap[B](afb: A => Parser[B]): Parser[B] =
        Parser(s => run(s).flatMap {
          case (s, a) => afb(a).run(s)
        })
    }
    object Parser {
      def get: Parser[Int] = Parser(s => Some(s, s))
      def put(s: Int): Parser[Unit] = Parser(_ => Some(s, ()))
      def modify(ss: Int => Int): Parser[Unit] = for { s <- get; _ <- put(ss(s)) } yield ()
    }

    val CurlyOpen = '{'
    val CurlyClose = '}'
    val SquareOpen = '['
    val SquareClose = ']'
    val Colon = ':'
    val Comma = ','
    val DoubleQuotes = '"'
    val BackSlash = '\\'
    val Space = ' '
    val LineFeed = '\n'
    val CarriageReturn = '\r'
    val Tab = '\t'
    val Escapable = Seq(DoubleQuotes, BackSlash, '/').map(c => c -> c).toMap ++ Map('b' -> '\b', 'f' -> '\f', 'n' -> LineFeed, 'r' -> CarriageReturn, 't' -> Tab)
    val EscapableUnicode = 'u'
    val Whitespaces = Set(Space, LineFeed, CarriageReturn, Tab)

    def parseDisjunction[T](parsers: Parser[T]*): Parser[T] = Parser(i => parsers.view.flatMap(parser => parser.run(i)).headOption)
    def parseConstantChar(constant: Char): Parser[Unit] = Parser(i => if(i < string.length && string(i) == constant) Some(i + 1, ()) else None)
    def parseConstant[T](constant: String, value: T): Parser[T] =
      Parser(i => if(i + constant.length <= string.length && string.substring(i, i + constant.length) == constant) Some(i + constant.length, value) else None)
    def parseWhitespaces: Parser[Unit] = Parser.modify(i => (i until string.length).dropWhile(string.andThen(Whitespaces.contains _)).headOption.getOrElse(string.length))
    def parseSeparated[T, S](valueParser: Parser[T], separatorParser: Parser[S]): Parser[IndexedSeq[T]] = Parser { i0 =>
      val sepParser = for {
        _ <- separatorParser
        value <- valueParser
      } yield value
      def loop(i: Int, acc: Seq[T]): (Int, IndexedSeq[T]) = {
        sepParser.run(i) match {
          case Some((i1, value)) => loop(i1, value +: acc)
          case None => (i, acc.reverse.toIndexedSeq)
        }
      }
      Some(valueParser.run(i0).map{ case (i, head) => loop(i, Seq(head)) }.getOrElse((i0, IndexedSeq.empty)))
    }
    def parseRegex(regex: String): Parser[String] = Parser { i =>
      import java.util.regex.Pattern
      val matcher = Pattern.compile(regex).matcher(string)
      if(matcher.find(i)) {
        if(matcher.start() == i) {
          val group = matcher.group(0)
          Some(i + group.length, group)
        } else {
          None
        }
      } else {
        None
      }
    }

    def parseString: Parser[String] = {
      def loop(i: Int, acc: Seq[Char]): Option[(Int, String)] = {
        import scala.util.Try
        if(i < string.length) {
          string(i) match {
            case DoubleQuotes => Some(i + 1, acc.reverse.mkString)
            case BackSlash =>
              if(i + 1 < string.length) {
                val c = string(i + 1)
                Escapable.get(c) match {
                  case Some(replacement) => loop(i + 2, replacement +: acc)
                  case None => c match {
                    case EscapableUnicode if i + 5 < string.length =>
                      val hex = string.substring(i + 2, i + 6).toLowerCase
                      Try(Integer.parseInt(hex, 16)).toOption match {
                        case Some(code) => loop(i + 6, code.toChar +: acc)
                        case None => None
                      }
                    case _ => None
                  }
                }
              } else {
                None
              }
            case c => loop(i + 1, c +: acc)
          }
        } else {
          None
        }
      }
      Parser(i => if(i + 1 < string.length && string(i) == DoubleQuotes) loop(i + 1, Seq.empty) else None)
    }
    def parsePrimitive: Parser[JsonPrimitive] = {
      parseDisjunction(
        parseString.map(JsonString),
        parseRegex("-?(?:[1-9][0-9]*|0)(?:\\.[0-9]+)?(?:[eE][-+]?[0-9]+)?").map(JsonNumber),
        parseConstant("true", JsonBoolean(true)),
        parseConstant("false", JsonBoolean(false)),
        parseConstant("null", JsonNull)
      )
    }
    def parseComma: Parser[Unit] = parseConstantChar(Comma)
    def parseObject: Parser[JsonObject] = for {
      _ <- parseConstantChar(CurlyOpen)
      _ <- parseWhitespaces
      keyValues <- parseSeparated(for {
        _ <- parseWhitespaces
        key <- parseString
        _ <- parseWhitespaces
        _ <- parseConstantChar(Colon)
        value <- parseValue
      } yield (key, value), parseComma)
      _ <- parseConstantChar(CurlyClose)
    } yield JsonObject(keyValues.toMap)
    def parseArray: Parser[JsonArray] = for {
      _ <- parseConstantChar(SquareOpen)
      _ <- parseWhitespaces
      values <- parseSeparated(parseValue, parseComma)
      _ <- parseConstantChar(SquareClose)
    } yield JsonArray(values)
    def parseNested: Parser[JsonNested] = parseDisjunction(parseObject, parseArray)
    def parseValue: Parser[JsonValue] = for {
      _ <- parseWhitespaces
      value <- parseDisjunction(parsePrimitive, parseNested)
      _ <- parseWhitespaces
    } yield value
    def parseTop: Parser[JsonNested] = for {
      _ <- parseWhitespaces
      top <- parseNested
      _ <- parseWhitespaces
    } yield top

    parseTop.run(0).collect { case (i, result) if i == string.length => result }.getOrElse(throw new Exception)
  }

}
