import sbt._
import Keys._

object AntwarBuild extends Build
{
  lazy val core = Project("core", file(".")) settings(
    name := "antwar",
    version := "1.0",
    scalaVersion := "2.9.1",
    libraryDependencies ++= Seq(
      "org.scala-tools.testing" %% "scalacheck" % "1.9",
      "org.scala-tools.testing" % "test-interface" % "0.5",
      "org.scalatest" % "scalatest_2.9.0" % "1.6.1"
    )
  )
}
