package hana.syntax

import fastparse._
import SingleLineWhitespace._
import hana.define.Literals
import hana.define.Literals._
import scala.collection.{Map => ScalaMap}

object Parser {
  private val keywords = Seq("def", "do", "end", "true", "false", "or", "not", "if", "else", "and", "use")

  /* We parse every line and obtain a tree of expressions from that. */
  def line[_: P]: P[Seq[Literals]] = P(tokenStart | comment).rep
  def expr[_: P]: P[Literals] = P(identifier | number | string | function | list | map)

  def string[_: P]: P[Str] = P("\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"").map(Str)
  def map[_: P]: P[Map] = P(occupiedMap | emptyMap)
  def list[_: P]: P[List] = P(occupiedList | emptyList)
  def number[_: P]: P[Num] = P(decimal | digits).!.map(str => Num(str.replace("_", "").toDouble))

  def identifier[_: P]: P[Ident] = P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_", 0)).!
    .filter(!keywords.contains(_))
    .map(Ident)

  def function[_: P]: P[Function] = P("def" ~~/ &(" ") ~/ identifier ~ functionArgs ~/ block).map {
    case (Ident(name), Some(args), body) => Function(name, args.map(_.name), body)
    case (Ident(name), _, body) => Function(name, Seq.empty, body)
  }

  private def functionArgs[_: P] = P("(" ~/ identifier.rep(sep = ",").? ~ ")").log
  private def block[_: P] = P("do" ~/ line ~ "end")
  private def decimal[_: P] = P(digits.? ~ "." ~ digits ~ !".")
  private def emptyMap[_: P] = P("{}").map(_ => Map(ScalaMap.empty))
  private def emptyList[_: P] = P("[]").map(_ => List(Seq.empty))
  private def occupiedMap[_: P] = P("{" ~/ (expr ~/ "->" ~/ expr).rep(sep = ",") ~ "}").map(kv => Map(kv.toMap))
  private def occupiedList[_: P] = P("[" ~/ expr.rep(sep = ",") ~ "]").map(List)
  private def digits[_: P] = P(CharIn("0-9") ~ (CharsWhileIn("0-9_") ~ !"_").?)
  private def comment[_: P] = P("#" ~ AnyChar.rep(0)).map(_ => Empty())

  private def tokenStart[_: P] = P((CharIn(";\n\f\r") | Start) ~ expr.?).map {
    case Some(expr) => expr
    case _ => Empty()
  }
}
