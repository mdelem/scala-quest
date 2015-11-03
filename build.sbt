name := """scala-quest"""

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)