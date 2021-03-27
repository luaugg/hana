package hana.define

import scala.collection.{Map => ScalaMap}

sealed trait Literals

object Literals {
  case class Str(str: String) extends Literals // string literals
  case class Num(number: Double) extends Literals // number literals, including decimals
  case class Ident(name: String) extends Literals // things like param names
  case class Map(map: ScalaMap[Literals, Literals]) extends Literals
  case class List(seq: Seq[_]) extends Literals
}