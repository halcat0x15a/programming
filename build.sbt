autoCompilerPlugins := true

scalaVersion in ThisBuild := "2.11.4"

lazy val root = project aggregate (macros, debug)

lazy val macros = project in file("macros")

lazy val debug = project in file("debug") dependsOn macros

resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
)

libraryDependencies in ThisBuild ++= Seq(
  "org.scalaz.stream" %% "scalaz-stream" % "0.4.1",
  "org.scala-lang.plugins" %% "scala-continuations-library" % "1.0.2",
  compilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.11.2" % "1.0.2"),
  "com.chuusai" %% "shapeless" % "2.0.0",
  "org.scalacheck" %% "scalacheck" % "1.11.6",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)

scalacOptions in ThisBuild ++= Seq(
  "-deprecation",
  "-feature",
  "-P:continuations:enable"
)
