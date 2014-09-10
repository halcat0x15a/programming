autoCompilerPlugins := true

scalaVersion := "2.11.1"

resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
)

libraryDependencies ++= Seq(
  "org.scalaz.stream" %% "scalaz-stream" % "0.4.1",
//  compilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin" % "1.0.2" cross CrossVersion.full),
  "com.chuusai" %% "shapeless" % "2.0.0"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
//  "-P:continuations:enable",
  "-language:higherKinds"
)
