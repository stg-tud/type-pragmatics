name := "veritas"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

libraryDependencies += "io.verizon.quiver" %% "core" % "5.4.12"

libraryDependencies += "org.jetbrains.xodus" % "xodus-entity-store" % "1.0.4"

// https://mvnrepository.com/artifact/org.scala-lang/scala-pickling_2.11
libraryDependencies += "org.scala-lang" % "scala-pickling_2.11" % "0.9.1"

assemblyJarName in assembly := "Veritas.jar"

test in assembly := {}

scalacOptions += "-deprecation"

scalacOptions += "-feature"
