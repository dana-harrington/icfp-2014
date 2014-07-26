name := """icfp_2014"""

version := "1.0"

scalaVersion := "2.11.1"

// Change this to another test framework if you prefer
//libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.6" % "test"
libraryDependencies += "org.specs2" %% "specs2" % "2.3.13" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

scalacOptions += "-feature"

