package hana.define

import fastparse._
import MultiLineWhitespace._
import Expr._

object Parser {
  def string[_: P]: P[Str] = P("\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"").map(Str)
  def identifier[_: P]: P[Ident] =  P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_", 0)).!.map(Ident)
  def expr[_: P]: P[Expr] = P(string | identifier | list | map | number)
  def map[_: P]: P[Map] = P("{" ~/ (expr ~ "->" ~/ expr).rep(0, ",") ~ "}").map(entries => Map(entries.toMap))
  def list[_: P]: P[List] = P("[" ~/ expr.rep(0, ", ") ~ "]").map(List)

  def number[_: P]: P[Num] = P(CharsWhileIn("0-9").!.? ~ ".".!.? ~ CharsWhileIn("0-9").!.?).map {
    case (_, Some(_), None) => throw new NumberFormatException("no fractional component in decimal literal")
    case (Some(integer), None, None) => Num(integer.toDouble)
    case (Some(integer), Some(_), Some(fraction)) => Num(integer.toDouble + fraction.toDouble / 10)
    case (_, _, Some(fraction)) => Num(fraction.toDouble / 10)
    case _ => throw new NumberFormatException("attempted number parsing but nothing to parse?")
  }
}