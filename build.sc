import mill._, scalalib._

object hana extends ScalaModule {
  def scalaVersion = "2.13.1"
  override def ivyDeps = Agg(ivy"com.lihaoyi::fastparse::2.2.2")

  object test extends Tests {
    override def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.7.7")
    override def testFrameworks = Seq("utest.runner.Framework")
  }
}