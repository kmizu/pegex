import sbt._
import Keys._

object PegexBuild extends Build {
  // Declare a project in the root directory of the build with ID "pegex".
  lazy val root = Project("pegex", file("."), settings = Defaults.defaultSettings ++ Seq(name := "pegex", version := "0.1"))
}
