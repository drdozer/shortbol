lazy val sharedSettings = Seq(
  scalaVersion := "2.11.6",
  organization := "uk.co.turingatemyhamster",
  version := "0.0.1")


lazy val core = crossProject.settings(
  name := "shortbol-core",
  libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.3.0",
  libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.2.1",
  libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.1",
  testFrameworks += new TestFramework("utest.runner.Framework"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
  ).settings(sharedSettings : _*)
  
lazy val coreJs = core.js.settings(
  libraryDependencies += "uk.co.turingatemyhamster" %%% "datatree-core" % "develop-0.2.1",
  libraryDependencies += "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % "7.1.3"
)

lazy val coreJVM = core.jvm.settings(packAutoSettings : _*).settings(
  resolvers += Resolver.sonatypeRepo("public"),
  libraryDependencies += "uk.co.turingatemyhamster" %% "datatree-core" % "develop-0.2.1",
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.3",
  libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"
)

lazy val server = crossProject.settings(
  name := "shortbol-server"
  ).settings(sharedSettings : _*).dependsOn(core)

lazy val serverJs = server.js

lazy val serverJvm = server.jvm.settings(
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-http-experimental" % "1.0-RC3"
  )
)

lazy val root = Project(
  id = "shortbol",
  base = file(".")) aggregate (serverJs, serverJvm)
