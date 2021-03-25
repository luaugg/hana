import mill._, scalalib._

object langtest extends ScalaModule {
  def scalaVersion = "2.13.1"

  override def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse::2.2.2"
  )
}