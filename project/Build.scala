import sbt._
import Keys._

object PegexBuild extends Build {
  // Declare a project in the root directory of the build with ID "pegex".
  lazy val root = Project("pegex", file("."), settings = Defaults.defaultSettings ++ Seq(name := "pegex", version := "0.1"))
  lazy val specsRepo = "org.specs2" % "specs2" % "2_2.9.1"
  libraryDependencies ++= Seq(
      "org.specs2" %% "specs2" % "2_2.9.1"
  )
}
