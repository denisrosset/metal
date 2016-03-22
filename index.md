---
layout: default
title:  "Home"
section: "home"
---
Metal is a Scala library to provide fast unboxed data structures.

<div class="msg warn"> <p><strong> Metal is currently an experimental
project under active development</strong>. Feedback and
contributions are welcomed as we look to improve the project. </p> </div>

<a name="getting-started"></a>

# Getting started

Metal is currently available for Scala 2.10 and 2.11.

```scala

resolvers += "bintray/denisrosset" at "http://dl.bintray.com/denisrosset/maven",

libraryDependencies ++= Seq(
   "org.scala-metal" %% "metal-core" % "VERSION",
   "org.scala-metal" %% "metal-library" % "VERSION"
)

```

where `VERSION` is the latest published version (see the [Releases](https://github.com/denisrosset/metal/releases)).
