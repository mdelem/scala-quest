name := """scala-quest"""

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.6.0"

scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions"
)
