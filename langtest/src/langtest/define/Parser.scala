package langtest.define

import fastparse._
import MultiLineWhitespace._

object Parser {
  def string[_: P] = P("\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"").map(Expr.Str)
  def number[_: P] = P(CharsWhileIn("0-9")).!.map(_.toDouble).map(Expr.Num)
  def identifier[_: P] = P(CharIn("a-zA-Z_") ~~ CharsWhileIn("a-zA-Z0-9_", 0)).!.map(Expr.Ident)
  def expr[_: P] = P(string | number | identifier)
}