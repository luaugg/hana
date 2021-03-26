import utest._
import hana.define.Parser.expr
import fastparse.{Parsed, parse}
import hana.define.Expr._

object ParserTests extends TestSuite {
  val tests: Tests = Tests {
    test("parse_string") {
      val Parsed.Success(Str(str), _) = parse("\"Hello, world!\"", expr(_))
      str ==> "Hello, world!"
    }

    test("parse_numbers") {
      test("parse_integer") {
        val Parsed.Success(Num(num), _) = parse("123", expr(_))
        num ==> 123
      }

      test("parse_big_integer") {
        val Parsed.Success(Num(result), _) = parse("1".repeat(400), expr(_))
        result ==> Double.PositiveInfinity
      }

      test("parse_decimal") {
        test("parse_decimal_with_two_points") {
          // todo: will silently ignore. add strict error messages
          val Parsed.Success(Num(result), _) = parse("1.5.2", expr(_))
          result
        }

        test("parse_decimal_at_end") {
          parse("1.", expr(_))
        }

        test("parse_normal_decimal") {
          val Parsed.Success(Num(num), _) = parse("1.5", expr(_))
          num ==> 1.5
        }

        test("parse_decimal_at_start") {
          val Parsed.Success(Num(num), _) = parse(".5", expr(_))
          num ==> .5
        }
      }
    }

    test("parse identifier") {
      val Parsed.Success(Ident(ident), _) = parse("_a1", expr(_))
      ident ==> "_a1"
    }

    test("parse map") {
      val Parsed.Success(Map(map), _) = parse("{abc -> 54321, \"foo\" -> bar}", expr(_))
      map(Str("foo")) ==> Ident("bar")
      map(Ident("abc")) ==> Num(54321)
    }

    test("parse list") {
      val Parsed.Success(List(seq), _) = parse("[1, \"foo\", bar, {}]", expr(_))
      seq.head ==> Num(1)
      seq(1) ==> Str("foo")
      seq(2) ==> Ident("bar")
      seq.tail ==> Map(scala.collection.Map.empty)
    }
  }
}