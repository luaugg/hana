import fastparse.{Parsed, parse}
import hana.define.Literals._
import hana.syntax.Parser._
import utest._

import scala.collection.{Map => ScalaMap}

object SuccessfulParserTests extends TestSuite {
  val tests: Tests = Tests {
    test("string") {
      val Parsed.Success(Str("Hello, world!"), _) = parse("\"Hello, world!\"", expr(_))
    }

    test("identifier") {
      val Parsed.Success(Ident("_a1"), _) = parse("_a1", expr(_))
    }

    test("numbers") {
      test("integer") {
        val Parsed.Success(Num(123), _) = parse("123", expr(_))
      }

      test("big_integer") {
        val Parsed.Success(Num(Double.PositiveInfinity), _) = parse("1".repeat(400), expr(_))
      }

      test("decimal") {
        test("parse_normal_decimal") {
          val Parsed.Success(Num(1.5), _) = parse("1.5", expr(_))
        }

        test("decimal_at_start") {
          val Parsed.Success(Num(.5), _) = parse(".5", expr(_))
        }
      }
    }

    test("maps") {
      test("empty_map") {
        val Parsed.Success(Map(map), _) = parse("{}", expr(_))
        map ==> ScalaMap.empty
      }

      test("occupied_map") {
        val Parsed.Success(Map(map), _) = parse("{abc -> 54321, \"foo\" -> bar}", expr(_))
        map(Str("foo")) ==> Ident("bar")
        map(Ident("abc")) ==> Num(54321)
      }
    }

    test("lists") {
      test("empty_list") {
        val Parsed.Success(List(seq), _) = parse("[]", expr(_))
        seq ==> Seq.empty
      }

      test("occupied_list") {
        val Parsed.Success(List(Seq(Num(1), Str("foo"), Ident("bar"), Map(map))), _) =
          parse("[1, \"foo\", bar, {}]", expr(_))

        map ==> ScalaMap.empty
      }
    }
  }
}
