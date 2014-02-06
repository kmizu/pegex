organization := "com.github.kmizu"

name := "pegex"

version := "0.3-SNAPSHOT"

scalaVersion := "2.10.3"

publishMavenStyle := true

val scaladocBranch = settingKey[String]("branch name for scaladoc -doc-source-url")

scaladocBranch := "master"

scalacOptions in (Compile, doc) ++= { Seq(
  "-sourcepath", baseDirectory.value.getAbsolutePath,
  "-doc-source-url", s"https://github.com/nscala-time/nscala-time/tree/${scaladocBranch.value}€{FILE_PATH}.scala"
)}

testOptions += Tests.Argument(TestFrameworks.Specs2, "console", "junitxml")

crossScalaVersions := Seq("2.9.1", "2.9.2", "2.9.3", "2.10.3")

scalacOptions <++= scalaVersion map { v =>
  if (v.startsWith("2.10"))
    Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions")
  else
    Seq("-unchecked", "-deprecation")
}

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.7" % "test"
)

libraryDependencies += {
  if (scalaVersion.value.startsWith("2.1"))
    "org.specs2" %% "specs2-junit" % "2.3.7" % "test"
  else if (scalaVersion.value == "2.9.3")
    "org.specs2" %% "specs2" % "1.12.4.1" % "test"
  else
    "org.specs2" %% "specs2" % "1.12.3" % "test"
}

initialCommands in console += {
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

publishTo <<= version { v =>
  val nexus = "http://oss.sonatype.org/"
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
