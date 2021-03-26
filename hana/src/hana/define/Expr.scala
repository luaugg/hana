package hana.define

import scala.collection.{Map => ScalaMap}

sealed trait Expr

object Expr {
  case class Str(str: String) extends Expr // string literals
  case class Num(number: Double) extends Expr // number literals, including decimals
  case class Ident(name: String) extends Expr // things like param names
  case class Map(map: ScalaMap[Expr, Expr]) extends Expr
  case class List(seq: Seq[_]) extends Expr
}