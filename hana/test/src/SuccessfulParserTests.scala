import hana.define.Expr._
import utest._
import TestUtils._
import hana.syntax.Parser._

import scala.collection.{Map => ScalaMap}

object SuccessfulParserTests extends TestSuite {
  val tests: Tests = Tests {
    test("string") { Str("Hello, world!") ==> extract("\"Hello, world!\"") }
    test("identifier") { Ident("_a1") ==> extract("_a1") }

    test("numbers") {
      test("integer") { Num(123) ==> extract("123")}
      test("big_integer") { Num(Double.PositiveInfinity) ==> extract("1".repeat(400))}
      test("decimal") { Num(0.500501) ==> extract(".500_501") }
    }

    test("maps") {
      test("empty") { Map(ScalaMap.empty) ==> extract("{}") }
      test("occupied") { Map(ScalaMap(Str("foo") -> Ident("bar"), Ident("abc") -> Num(54321))) ==>
        extract("{abc -> 54321, \"foo\" -> bar}") }
    }

    test("lists") {
      test("empty") { List(Seq.empty) ==> extract("[]") }
      test("occupied") { List(Seq(Num(1), Str("foo"), Ident("bar"), Map(ScalaMap.empty))) ==>
        extract("[1, \"foo\", bar, {}]") }
    }

    test("multiple_lines") {
      Seq(Map(ScalaMap.empty), Empty(), List(Seq.empty), Num(123)) ==> extract("{};;[]\n123", line(_))
    }

    /*
    test("functions") {
      test("args") { Function("a", Seq("b"), Seq(Empty())) ==> extract("def a(b) do ; end", function(_)) }
      test("no_args") { Function("a", Seq.empty, Seq(Empty())) ==> extract("def a() do ; end", function(_)) }

      test("calls") {
        test("no_args") { Call("a", Seq.empty) ==> extract("a()") }
        test("args") { Call("a", Seq(Num(123))) ==> extract("a(123)") }
      }
    }
    test("match") {
      Match("a", Match("b", Num(123))) ==> extract("a=b=123", `match`(_))
    }
    */
  }
}
