autoCompilerPlugins := true

scalaVersion := "2.10.4"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "org.scalaz.stream" %% "scalaz-stream" % "0.4.1",
  compilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.4")
)

scalacOptions ++= Seq("-deprecation", "-feature", "-language:higherKinds", "-P:continuations:enable")
