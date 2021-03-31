package hana.syntax

import fastparse._
import SingleLineWhitespace._
import hana.define.Expr
import hana.define.Expr._
import hana.exceptions.ParseException

object Parser {
  import Tokens._

  private val keywords = Seq("def", "do", "end", "true", "false", "or", "not", "if", "else", "and", "use")

  def identifier[_: P]: P[Ident] = P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_", 0)).!
    .filter(!keywords.contains(_))
    .map(Ident)

  def string[_: P]: P[Str] = P("\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"").map(Str)
  def number[_: P]: P[Num] = P(decimal | digits).!.map(str => Num(str.replace("_", "").toDouble))
  def map[_: P]: P[Map] = P("{" ~/ (expr ~/ ("->" | "to") ~/ expr).rep(0, ",") ~ "}").map(kv => Map(kv.toMap))
  def list[_: P]: P[List] = P("[" ~/ expr.rep(0, ",") ~ "]").map(List)
  def function[_: P]: P[Function] = P("def" ~/ S ~ identifier ~/ "(" ~/ identifier.rep(0, ", ") ~ ")" ~/ block).map {
    case (Ident(name), args, body) => Function(name, args.map(_.name), body)
  }

  def expr[_: P]: P[Expr] = P((number | identifier | string | function | list | map) ~/ suffixToken.?).map {
    case (Ident(name), Some(MatchToken(right))) => Match(name, right)
    case (Ident(name), Some(CallToken(args))) => Call(name, args)
    case (Function, Some(_)) => throw ParseException("suffix expression following a function is not allowed")
    case (exp, _) => exp
  }

  def `match`[_: P]: P[MatchToken] = P("=" ~/ expr).map(MatchToken)
  def call[_: P]: P[CallToken] = P("(" ~/ expr.rep(0, ",") ~ ")").map(CallToken)

  def suffixToken[_: P]: P[Tokens] = P(`match` | call)
  def line[_: P]: P[Seq[Expr]] = P(tokenStart | comment).rep

  private def decimal[_: P] = P(digits.? ~ "." ~ digits ~ !".")
  private def digits[_: P] = P(CharIn("0-9") ~ (CharsWhileIn("0-9_") ~ !"_").?)
  private def block[_: P]: P[Seq[Expr]] = P("do" ~/ S ~ line ~ "end")
  private def comment[_: P] = P("#" ~ AnyChar.rep(0)).map(_ => Empty())
  private def tokenStart[_: P] = P((CharIn(";\n\f\r") | Start) ~ expr.?).map {
    case Some(expr) => expr
    case _ => Empty()
  }
  private def S[_: P] = P(&(" "))
}

sealed trait Tokens

object Tokens {
  case class MatchToken(right: Expr) extends Tokens
  case class CallToken(right: Seq[Expr]) extends Tokens
}