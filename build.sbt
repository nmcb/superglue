ThisBuild / scalaVersion := "3.7.4"
ThisBuild / version      := "0.1.0"

ThisBuild / libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-jdk14" % "2.0.9",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.20.1",
  "com.jayway.jsonpath" % "json-path" % "2.10.0",

  "org.scalatest"  %% "scalatest"  % "3.2.19" % "test"
)

ThisBuild / scalacOptions ++= Seq(
  "-encoding", "utf8",
  "-feature",
  "-language:implicitConversions",
  "-language:existentials",
  "-unchecked",
  "-Werror",
  "-deprecation"
)

ThisBuild / Compile / run / fork         := true
ThisBuild / Compile / run / javaOptions ++= Seq("-Xmx4G", "-Xss1G", "-XX:+UseG1GC")

ThisBuild / Test / fork         := true
ThisBuild / Test / javaOptions ++= Seq("-Xmx4G", "-Xss1G", "-XX:+UseG1GC")
ThisBuild / Test / testOptions  += Tests.Argument("-oD")

Global / onChangedBuildSource   := ReloadOnSourceChanges

lazy val core = project in file("core")

lazy val examples = (project in file("examples"))
  .dependsOn(core)

lazy val superglue = (project in file("."))
  .aggregate(
    core,
    examples
  )
