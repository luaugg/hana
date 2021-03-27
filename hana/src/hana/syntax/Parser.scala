package hana.syntax

import fastparse._
import SingleLineWhitespace._
import hana.define.Literals
import hana.define.Literals._
import scala.collection.{Map => ScalaMap}

object Parser {
  private val keywords = Seq("def", "do", "end", "true", "false", "or", "not", "if", "else", "and", "use")

  def expr[_: P]: P[Literals] = P(string | identifier | list | map | number)
  def string[_: P]: P[Str] = P("\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"").map(Str)
  def map[_: P]: P[Map] = P(emptyMap | occupiedMap)
  def list[_: P]: P[List] = P(emptyList | occupiedList)
  def number[_: P]: P[Num] = P(digits.? ~ ("." ~ digits ~ !".").?).!.map(str => Num(str.toDouble))
  def identifier[_: P]: P[Ident] = P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_", 0)).!
    .filter(!keywords.contains(_))
    .map(Ident)


  private def emptyMap[_: P] = P("{}").map(_ => Map(ScalaMap.empty))
  private def emptyList[_: P] = P("[]").map(_ => List(Seq.empty))
  private def occupiedMap[_: P] = P("{" ~/ (expr ~/ "->" ~/ expr).rep(0, ",") ~ "}").map(entries => Map(entries.toMap))
  private def occupiedList[_: P] = P("[" ~/ expr.rep(0, ",") ~ "]").map(List)
  private def digits[_: P] = P(CharsWhileIn("0-9"))
}
