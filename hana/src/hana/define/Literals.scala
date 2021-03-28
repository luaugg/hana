package hana.define

import scala.collection.{Map => ScalaMap}

sealed trait Literals

object Literals {
  case class Str(str: String) extends Literals // string literals
  case class Num(number: Double) extends Literals // number literals, including decimals
  case class Ident(name: String) extends Literals // things like param names
  case class Map(map: ScalaMap[Literals, Literals]) extends Literals
  case class List(seq: Seq[_]) extends Literals
  case class Empty() extends Literals // A line without any valid tokens. Filtered when evaluating.

  case class Function(name: String, args: Seq[String], body: Seq[Literals]) extends Literals
  case class Call(name: String, args: Seq[Literals]) extends Literals
  case class Match(name: String, value: Literals) extends Literals
}