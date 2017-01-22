name := "transform-typing"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "default" %% "veritas" % "1.1"

libraryDependencies += "veritas-benchmarking" %% "veritas-benchmarking" % "1.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

scalacOptions ++= Seq("-deprecation", "-feature")