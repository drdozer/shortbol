scalaVersion := "2.11.6"

name := "shortbol"

version := "0.0.1"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)

libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.3.0"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.0"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.1.7"
