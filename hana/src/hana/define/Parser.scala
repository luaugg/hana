package hana.define

import fastparse._
import MultiLineWhitespace._

object Parser {
  def string[_: P] = P("\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"").map(Expr.Str)
  def identifier[_: P] = P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_", 0)).!.map(Expr.Ident)
  def expr[_: P] = P(string | number | identifier)

  def number[_: P] = P(integer0.!.? ~ ".".!.? ~ integer0.!.?).map {
    case (_, Some(_), None) => throw new NumberFormatException("no fractional component in decimal literal")
    case (Some(integer), None, None) => Expr.Num(integer.toDouble)
    case (Some(integer), Some(_), Some(fraction)) => Expr.Num(integer.toDouble + fraction.toDouble / 10)
    case (_, _, Some(fraction)) => Expr.Num(fraction.toDouble / 10)
    case _ => throw new NumberFormatException("attempted number parsing but nothing to parse?")
  }

  def integer0[_: P] = P(CharsWhileIn("0-9"))
}