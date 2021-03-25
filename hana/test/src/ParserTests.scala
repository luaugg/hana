import utest._
import hana.define.Parser._
import fastparse.{Parsed, parse}
import hana.define.Expr

object ParserTests extends TestSuite {
  val tests = Tests {
    test("parse string") {
      val Parsed.Success(Expr.Str("Hello, world!"), _) = parse("\"Hello, world!\"", string(_))
    }

    test("parse numbers") {
      test("parse integer") {
        val Parsed.Success(Expr.Num(123), _) = parse("123", number(_))
      }

      test("parse big integer") {
        val Parsed.Success(Expr.Num(Double.PositiveInfinity), _) = parse("1".repeat(400), number(_))
      }

      test("parse decimal") {
        val Parsed.Success(Expr.Num(1.5), _) = parse("1.5", number(_))
        // fails with 1.0 (stops at non-numeric)
      }
    }
  }
}