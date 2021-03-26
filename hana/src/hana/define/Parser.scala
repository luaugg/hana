package hana.define

import fastparse._
import MultiLineWhitespace._
import Expr._
import scala.collection.{Map => ScalaMap}

object Parser {
  def string[_: P]: P[Str] = P("\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"").map(Str)
  def identifier[_: P]: P[Ident] =  P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_", 0)).!.map(Ident)
  def map[_: P]: P[Map] = P(emptyMap | occupiedMap)
  def list[_: P]: P[List] = P(emptyList | occupiedList)
  def expr[_: P]: P[Expr] = P(string | identifier | list | map | number)

  def number[_: P]: P[Num] = P(CharsWhileIn("0-9").!.? ~ ".".!.? ~ CharsWhileIn("0-9").!.?).map {
    case (_, Some(_), None) => throw new NumberFormatException("no fractional component in decimal literal")
    case (Some(integer), None, None) => Num(integer.toDouble)
    case (Some(integer), Some(_), Some(fraction)) => Num(integer.toDouble + fraction.toDouble / 10)
    case (_, _, Some(fraction)) => Num(fraction.toDouble / 10)
    case _ => throw new NumberFormatException("attempted number parsing but nothing to parse?")
  }

  private def emptyMap[_: P] = P("{}").map(_ => Map(ScalaMap.empty))
  private def emptyList[_: P] = P("[]").map(_ => List(Seq.empty))
  private def occupiedMap[_: P] = P("{" ~/ (expr ~ "->" ~/ expr).rep(0, ",") ~ "}").map(entries => Map(entries.toMap))
  private def occupiedList[_: P] = P("[" ~/ expr.rep(0, ", ") ~ "]").map(List)
}