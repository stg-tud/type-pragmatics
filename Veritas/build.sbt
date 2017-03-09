name := "veritas"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += "io.verizon.quiver" %% "core" % "5.4.12"

libraryDependencies += "org.jetbrains.xodus" % "xodus-entity-store" % "1.0.4"

assemblyJarName in assembly := "Veritas.jar"

test in assembly := {}