package langtest.define

sealed trait Expr

object Expr {
  case class Str(s: String) extends Expr // string literals
  case class Num(n: Double) extends Expr // all numbers will be represented as doubles
  case class Ident(name: String) extends Expr // things like param names
}