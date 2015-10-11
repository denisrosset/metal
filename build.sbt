lazy val commonSettings = Seq(
  scalaVersion := "2.11.7",
  organization := "com.faacets",
  libraryDependencies ++= Seq(
    "org.spire-math" %% "spire" % "0.10.1"
  ),
  scalacOptions ++= Seq(
    "-Yinline-warnings",
    "-deprecation",
    "-unchecked",
    "-optimize",
    "-language:experimental.macros",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-feature"
  ),
  version := "0.0.1"
)

lazy val scalaReflect = Def.setting { "org.scala-lang" % "scala-reflect" % scalaVersion.value }

lazy val library = (project in file("library")).
  dependsOn(core).
  settings(commonSettings: _*).
  settings(
    // other settings here
  )

lazy val core = (project in file("core")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies += scalaReflect.value
    // other settings here
  )
