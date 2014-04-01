autoCompilerPlugins := true

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "org.scalaz.stream" %% "scalaz-stream" % "0.1",
  compilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.0")
)

scalacOptions ++= Seq("-deprecation", "-feature", "-language:higherKinds", "-P:continuations:enable")
