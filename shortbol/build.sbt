
lazy val core = crossProject.settings(
  scalaVersion := "2.11.6",
  organization := "uk.co.turingatemyhamster",
  name := "shortbol-core",
  version := "0.0.1",
  libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.3.0",
  libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.1.7",
  libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.1",
  testFrameworks += new TestFramework("utest.runner.Framework"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
  )
  
lazy val coreJs = core.js
lazy val coreJVM = core.jvm.settings(packAutoSettings : _*)

