libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "0.1"

scalacOptions += "-deprecation"

initialCommands in console := """
import scalaz._, Scalaz._
import scalaz.stream._, Process._
"""
