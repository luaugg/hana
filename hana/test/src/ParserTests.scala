import fastparse.{Parsed, parse}
import hana.define.Expr._
import hana.define.Parser.{expr, list}
import utest._
import scala.collection.{Map => ScalaMap}

object ParserTests extends TestSuite {
  val tests: Tests = Tests {
    test("parse_string") {
      val Parsed.Success(Str("Hello, world!"), _) = parse("\"Hello, world!\"", expr(_))
    }

    test("parse_identifier") {
      val Parsed.Success(Ident("_a1"), _) = parse("_a1", expr(_))
    }

    test("parse_numbers") {
      test("parse_integer") {
        val Parsed.Success(Num(123), _) = parse("123", expr(_))
      }

      test("parse_big_integer") {
        val Parsed.Success(Num(Double.PositiveInfinity), _) = parse("1".repeat(400), expr(_))
      }

      test("parse_decimal") {
        test("parse_normal_decimal") {
          val Parsed.Success(Num(1.5), _) = parse("1.5", expr(_))
        }

        test("parse_decimal_at_start") {
          val Parsed.Success(Num(.5), _) = parse(".5", expr(_))
        }
      }
    }

    test("parse_maps") {
      test("parse_empty_map") {
        val Parsed.Success(Map(map), _) = parse("{}", expr(_))
        map ==> ScalaMap.empty
      }

      test("parse_occupied_map") {
        val Parsed.Success(Map(map), _) = parse("{abc -> 54321, \"foo\" -> bar}", expr(_))
        map(Str("foo")) ==> Ident("bar")
        map(Ident("abc")) ==> Num(54321)
      }
    }

    test("parse_lists") {
      test("parse_empty_list") {
        val Parsed.Success(List(seq), _) = parse("[]", expr(_))
        seq ==> Seq.empty
      }

      test("parse_occupied_list") {
        val Parsed.Success(List(Seq(Num(1), Str("foo"), Ident("bar"), Map(map))), _) =
          parse("[1, \"foo\", bar, {}]", expr(_))

        map ==> ScalaMap.empty
      }
    }
  }
}
