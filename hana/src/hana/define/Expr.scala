package hana.define

import scala.collection.{Map => ScalaMap}

sealed trait Expr

object Expr {
  case class Str(str: String) extends Expr // string literals
  case class Num(number: Double) extends Expr // number literals, including decimals
  case class Ident(name: String) extends Expr // things like param names
  case class Map(map: ScalaMap[Expr, Expr]) extends Expr
  case class List(seq: Seq[_]) extends Expr
  case class Empty() extends Expr // A line without any valid tokens. Filtered when evaluating.

  case class Function(name: String, args: Seq[Expr], body: Seq[Expr]) extends Expr
  case class Call(name: String, args: Seq[Expr]) extends Expr
  case class Match(name: String, value: Expr) extends Expr
}