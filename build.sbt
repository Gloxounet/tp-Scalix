ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "TP5"
  )

libraryDependencies += "org.json4s" %% "json4s-ast" % "4.0.6"
libraryDependencies += "org.json4s" %% "json4s-native" % "4.0.6"
libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.13.4"
libraryDependencies += "com.lihaoyi" %% "upickle" % "2.0.0"
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.8.1"
