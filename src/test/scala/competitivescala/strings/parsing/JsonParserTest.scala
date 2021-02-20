package competitivescala.strings.parsing

import JsonParser._
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Try

class JsonParserTest extends AnyFunSuite {

  test("json parsing") {
    def parseOpt(string: String): Option[JsonNested] = Try(parseJson(string)).toOption
    assert(parseOpt("").isEmpty)
    assert(parseOpt("a").isEmpty)
    assert(parseOpt("10").isEmpty)
    assert(parseOpt("{[]}").isEmpty)
    assert(parseOpt("""{"a":"b""}""").isEmpty)

    assert(parseJson("{}") == JsonObject(Map.empty))
    assert(parseJson("[]") == JsonArray(IndexedSeq.empty))
    assert(parseJson("""{"a":null}""") == JsonObject(Map("a" -> JsonNull)))
    assert(parseJson("""[null]""") == JsonArray(IndexedSeq(JsonNull)))
    assert(parseJson(""" { "a" : "b" ,   "c":null , "d" :42, " spaced ": false }  """) == JsonObject(Map("a" -> JsonString("b"), "c" -> JsonNull, "d" -> JsonNumber("42"), " spaced " -> JsonBoolean(false))))
    assert(parseJson("""{"a":{"b":"c"},"d":[{},{"e":[]}],"f":{"g":{}}}""") == JsonObject(Map("a" -> JsonObject(Map("b" -> JsonString("c"))), "d" -> JsonArray(IndexedSeq(JsonObject(Map.empty), JsonObject(Map("e" -> JsonArray(IndexedSeq.empty))))), "f" -> JsonObject(Map("g" -> JsonObject(Map.empty))))) )
  }

}
