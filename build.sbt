organization := "com.github.kmizu"

name := "pegex"

def Scala213 = "2.13.16"

scalaVersion := "3.3.6"

crossScalaVersions := Seq(scalaVersion.value, Scala213)

publishMavenStyle := true

val scaladocBranch = settingKey[String]("branch name for scaladoc -doc-source-url")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
  "junit" % "junit" % "4.13" % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

Test / fork := true

console / initialCommands += {
  Iterator().map("import "+).mkString("\n")
}

pomExtra := (
  <url>https://github.com/kmizu/pegex</url>
  <licenses>
    <license>
      <name>The MIT License</name>
      <url>http://www.opensource.org/licenses/MIT</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:kmizu/pegex.git</url>
    <connection>scm:git:git@github.com:kmizu/pegex.git</connection>
  </scm>
  <developers>
    <developer>
      <id>kmizu</id>
      <name>Kota Mizushima</name>
      <url>https://github.com/kmizu</url>
    </developer>
  </developers>
)

publishTo := {
  val v = version.value
  val nexus = "https://oss.sonatype.org/"
  if (v.endsWith("-SNAPSHOT"))
    Some("snapshots" at nexus+"content/repositories/snapshots")
  else
    Some("releases" at nexus+"service/local/staging/deploy/maven2")
}

credentials ++= {
  val sonatype = ("Sonatype Nexus Repository Manager", "oss.sonatype.org")
  def loadMavenCredentials(file: java.io.File) : Seq[Credentials] = {
    xml.XML.loadFile(file) \ "servers" \ "server" map (s => {
      val host = (s \ "id").text
      val realm = if (host == sonatype._2) sonatype._1 else "Unknown"
      Credentials(realm, host, (s \ "username").text, (s \ "password").text)
    })
  }
  val ivyCredentials   = Path.userHome / ".ivy2" / ".credentials"
  val mavenCredentials = Path.userHome / ".m2"   / "settings.xml"
  (ivyCredentials.asFile, mavenCredentials.asFile) match {
    case (ivy, _) if ivy.canRead => Credentials(ivy) :: Nil
    case (_, mvn) if mvn.canRead => loadMavenCredentials(mvn)
    case _ => Nil
  }
}
