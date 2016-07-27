lazy val sharedSettings = Seq(
  scalaVersion := "2.11.8",
  organization := "uk.co.turingatemyhamster",
  version := "0.0.1")

lazy val ast = crossProject.settings(
  name := "shortbol-ast",
  libraryDependencies += "com.lihaoyi" %%% "utest" % "0.4.3",
  testFrameworks += new TestFramework("utest.runner.Framework")
).settings(sharedSettings : _*)

lazy val astJs = ast.js

lazy val astJVM = ast.jvm

lazy val core = crossProject.settings(
  name := "shortbol-core",
  libraryDependencies += "com.chuusai" %%% "shapeless" % "2.3.0",
  libraryDependencies += "com.github.julien-truffaut"  %%%  "monocle-core" % "1.2.2",
  libraryDependencies += "com.github.julien-truffaut"  %%%  "monocle-macro" % "1.2.2",
  libraryDependencies += "com.lihaoyi" %%% "fastparse" % "0.3.7",
  libraryDependencies += "com.lihaoyi" %%% "utest" % "0.4.3" % "test",
  libraryDependencies += "uk.co.turingatemyhamster" %%% "datatree-core" % "0.2.2",
  testFrameworks += new TestFramework("utest.runner.Framework"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  ).settings(sharedSettings : _*).dependsOn(ast)
  
lazy val coreJs = core.js.settings(
  libraryDependencies += "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % "7.2.0",
  libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.5.6"
)

lazy val coreJVM = core.jvm.settings(packAutoSettings : _*).settings(
  resolvers += Resolver.sonatypeRepo("public"),
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.0",
  libraryDependencies += "com.github.scopt" %% "scopt" % "3.4.0",
  libraryDependencies += "com.lihaoyi" % "ammonite-repl" % "0.6.2" % "test" cross CrossVersion.full,
  libraryDependencies += "org.sbolstandard" % "libSBOLj" % "2.1.0" % "test",
  initialCommands in (Test, console) := """ammonite.repl.Main().run()"""
)

lazy val server = crossProject.settings(
  name := "shortbol-server"
  ).settings(sharedSettings : _*).dependsOn(core)

lazy val serverJs = server.js.settings(
  persistLauncher in Compile := false,
  persistLauncher in Test := false,
  libraryDependencies += "io.github.widok" %%% "widok" % "0.3.0-SNAPSHOT"
)

lazy val serverJvm = server.jvm
  .enablePlugins(SbtWeb, sbtdocker.DockerPlugin, JavaAppPackaging)
  .dependsOn(serverJs).settings(Revolver.settings: _*).settings(
  libraryDependencies += "com.typesafe.akka" %% "akka-http-experimental" % "2.4.7",
  (resources in Assets) += {
    (fastOptJS in (serverJs, Compile)).value
    (artifactPath in (serverJs, Compile, fastOptJS)).value
  },
//  (resources in Assets) += {
//    (fastOptJS in (serverJs, Compile)).value
//    (artifactPath in (serverJs, Compile, packageScalaJSLauncher)).value
//  },
  (resources in Assets) += {
    (fastOptJS in (serverJs, Compile)).value
    (artifactPath in (serverJs, Compile, packageJSDependencies)).value
  },
  (managedClasspath in Runtime) += (packageBin in Assets).value,

  dockerfile in docker := {
    val appDir: File = stage.value
    val targetDir = "/app"

    new Dockerfile {
      from("java:8-jre")
      entryPoint(s"$targetDir/bin/${executableScriptName.value}")
      copy(appDir, targetDir)
      expose(10080)
    }
  }
)

lazy val root = Project(
  id = "shortbol",
  base = file(".")) aggregate (serverJs, serverJvm)
