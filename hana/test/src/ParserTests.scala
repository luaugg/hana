import utest._
import hana.define.Parser.expr
import fastparse.{Parsed, parse}
import hana.define.Expr

object ParserTests extends TestSuite {
  val tests = Tests {
    test("parse_string") {
      val Parsed.Success(Expr.Str(str), _) = parse("\"Hello, world!\"", expr(_))
      str ==> "Hello, world!"
    }

    test("parse_numbers") {
      test("parse_integer") {
        val Parsed.Success(Expr.Num(num), _) = parse("123", expr(_))
        num ==> 123
      }

      test("parse_big_integer") {
        val Parsed.Success(Expr.Num(result), _) = parse("1".repeat(400), expr(_))
        result ==> Double.PositiveInfinity
      }

      test("parse_decimal") {
        test("parse_decimal_with_two_points") {
          // todo: will silently ignore. add strict error messages
          val Parsed.Success(Expr.Num(result), _) = parse("1.5.2", expr(_))
          result
        }

        test("parse_decimal_at_end") {
          parse("1.", expr(_))
        }

        test("parse_normal_decimal") {
          val Parsed.Success(Expr.Num(num), _) = parse("1.5", expr(_))
          num ==> 1.5
        }

        test("parse_decimal_at_start") {
          val Parsed.Success(Expr.Num(num), _) = parse(".5", expr(_))
          num ==> .5
        }
      }
    }

    test("parse identifier") {
      val Parsed.Success(Expr.Ident(ident), _) = parse("_a1", expr(_))
      ident ==> "_a1"
    }
  }
}