val scala3Version = "3.1.0"


lazy val root = project
  .in(file("."))
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "AOC",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    assembly / assemblyJarName := "main.jar",
  )

