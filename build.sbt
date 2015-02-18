scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  "org.typelevel" %% "machinist" % "0.3.0",
  "org.spire-math" %% "spire" % "0.9.1"
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
