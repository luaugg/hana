import hana.syntax.Parser.expr
import fastparse._

object TestUtils {
  type Parser = P[_] => P[_]

  def extract[T](code: String, parser: Parser = expr(_)): T = {
    val Parsed.Success(value, _) = parse(code, parser)
    value.asInstanceOf[T]
  }
}
