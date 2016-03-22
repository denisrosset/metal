val scalaCheckVersion = "1.12.4"
val scalaTestVersion = "3.0.0-M7"
val spireVersion = "0.11.0"

// inspired by Spire build.sbt file

lazy val metal = (project in file("."))
  .settings(moduleName := "metal")
  .settings(metalSettings: _*)
  .settings(noPublishSettings)
  .aggregate(core, library)
  .dependsOn(core, library)

lazy val core = (project in file("core"))
  .settings(moduleName := "metal-core")
  .settings(metalSettings: _*)
  .settings(coreSettings: _*)
  .settings(crossVersionSharedSources:_*)
  .settings(commonJvmSettings:_*)

lazy val library = (project in file("library"))
  .settings(moduleName := "metal-library")
  .settings(metalSettings: _*)
  .settings(coreSettings: _*)
  .settings(scalaTestSettings:_*)
  .settings(libraryDependencies += "org.scalacheck" %% "scalacheck" % scalaCheckVersion)
  .settings(crossVersionSharedSources:_*)
  .settings(commonJvmSettings:_*)
  .dependsOn(core)

lazy val metalSettings = buildSettings ++ commonSettings ++ publishSettings

lazy val buildSettings = Seq(
  organization := "org.scala-metal",
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.10.6", "2.11.8")
)

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions.diff(Seq(
    "-Xfatal-warnings", 
    "-language:existentials",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  )),
  resolvers ++= Seq(
    "bintray/non" at "http://dl.bintray.com/non/maven",
    Resolver.sonatypeRepo("snapshots")
  ),
  libraryDependencies += "org.spire-math" %% "spire" % spireVersion
) ++ scalaMacroDependencies ++ warnUnusedImport

lazy val coreSettings = Seq(
  buildInfoKeys := Seq[BuildInfoKey](version),
  buildInfoPackage := "metal"
)

lazy val publishSettings = Seq(
  homepage := None, // Some(url("http://scala-metal.org")),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  bintrayRepository := "maven",
  publishArtifact in Test := false
)

lazy val commonJvmSettings = Seq(
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
) ++ selectiveOptimize
  // -optimize has no effect in scala-js other than slowing down the build

// do not optimize on Scala 2.10 because of optimizer bug, see SI-3882
lazy val selectiveOptimize = 
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) =>
        Seq()
      case Some((2, n)) if n >= 11 =>
        Seq("-optimize")
    }
  }

lazy val scalaTestSettings = Seq(
  libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

////////////////////////////////////////////////////////////////////////////////////////////////////
// Base Build Settings - Should not need to edit below this line. 
//
// Taken from the common keys acrros various Typelevel projects, see e.g. cats

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val crossVersionSharedSources: Seq[Setting[_]] =
  Seq(Compile, Test).map { sc =>
    (unmanagedSourceDirectories in sc) ++= {
      (unmanagedSourceDirectories in sc ).value.map {
        dir:File => new File(dir.getPath + "_" + scalaBinaryVersion.value)
      }
    }
  }

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq()
      // in Scala 2.10, quasiquotes are provided by macro paradise
      case Some((2, 10)) =>
        Seq(
          compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full),
              "org.scalamacros" %% "quasiquotes" % "2.0.1" cross CrossVersion.binary
        )
    }
  }
)

lazy val warnUnusedImport = Seq(
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) =>
        Seq()
      case Some((2, n)) if n >= 11 =>
        Seq("-Ywarn-unused-import")
    }
  },
  scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},
  scalacOptions in (Test, console) <<= (scalacOptions in (Compile, console))
)
