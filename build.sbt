scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  "org.typelevel" %% "machinist" % "0.3.0",
  "org.spire-math" %% "spire" % "0.9.1",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"
)

scalacOptions ++= Seq(
  "-Yinline-warnings",
  "-deprecation",
  "-unchecked",
  "-optimize",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-feature"
)
