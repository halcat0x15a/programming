autoCompilerPlugins := true

scalaVersion in ThisBuild := "2.11.5"

lazy val root = project aggregate (macros, debug)

lazy val macros = project in file("macros")

lazy val debug = project in file("debug") dependsOn macros

resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
  "maven central" at "http://central.maven.org/maven2/"
)

libraryDependencies ++= Seq(
  "org.scalaz.stream" %% "scalaz-stream" % "0.6a",
  "org.scala-lang.plugins" %% "scala-continuations-library" % "1.0.2",
  compilerPlugin("org.scala-lang.plugins" % s"scala-continuations-plugin_${scalaVersion.value}" % "1.0.2"),
  "com.chuusai" %% "shapeless" % "2.0.0",
  "org.scalacheck" %% "scalacheck" % "1.11.6",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.openjdk.jmh" % "jmh-core" % "1.5"
    
)

scalacOptions in ThisBuild ++= Seq(
  "-deprecation",
  "-feature"
)
