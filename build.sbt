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

lazy val crossVersionSharedSources: Seq[Setting[_]] =
  Seq(Compile, Test).map { sc =>
    (unmanagedSourceDirectories in sc) ++= {
      (unmanagedSourceDirectories in sc ).value.map {
        dir:File => new File(dir.getPath + "_" + scalaBinaryVersion.value)
      }
    }
  }

lazy val commonSettings = Seq(
  scalaVersion := "2.11.7",
  organization := "com.faacets",
  libraryDependencies ++= Seq(
    "org.spire-math" %% "spire" % "0.10.1",
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"
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
  settings(crossVersionSharedSources:_*).
  settings(
    libraryDependencies += scalaReflect.value
    // other settings here
  )
