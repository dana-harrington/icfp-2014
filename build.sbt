name := """icfp_2014"""

version := "1.0"

scalaVersion := "2.11.1"

// Change this to another test framework if you prefer
//libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.6" % "test"
libraryDependencies += "org.specs2" %% "specs2" % "2.3.13" % "test"

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.3"

scalacOptions += "-feature"

