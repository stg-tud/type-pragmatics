name := "transform-typing"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "default" %% "veritas" % "1.1"

libraryDependencies += "veritas-benchmarking" %% "veritas-benchmarking" % "1.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += "org.sameersingh.scalaplot" % "scalaplot" % "0.0.4"

scalacOptions ++= Seq("-deprecation", "-feature")

import com.typesafe.sbt.SbtStartScript

seq(SbtStartScript.startScriptForClassesSettings: _*)
