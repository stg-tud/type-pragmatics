name := "veritas"

version := "1.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"

libraryDependencies += "org.jetbrains.xodus" % "xodus-entity-store" % "1.2.3"

libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "2.1.0"

libraryDependencies += "org.scalameta" %% "scalameta" % "4.1.0"

libraryDependencies += "ch.epfl.scala" % "scalafix-core_2.12" % "0.9.4"

unmanagedJars in Compile += file("lib/scalaz3_2.11-3.0.jar")

assemblyJarName in assembly := "Veritas.jar"

test in assembly := {}

scalacOptions += "-deprecation"

scalacOptions += "-feature"

fork in Test := true
