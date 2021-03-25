import utest._
import hana.define.Parser._
import fastparse.parse

object ParserTests extends TestSuite {
  val tests = Tests {
    test("parse string") {
      parse("\"Hello, world!", string(_))
    }
  }
}