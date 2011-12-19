import sbt._
import Keys._

object XMLDiff extends Build {
  val projectSettings = Defaults.defaultSettings ++ Seq(
    name := "xml-diff",
    version := "0.3",
    organization := "com.google",
    scalaVersion := "2.9.1",
    crossScalaVersions := Seq("2.8.0", "2.8.1", "2.8.2", "2.9.0", "2.9.0-1", "2.9.1"),
    scalaSource in Compile <<= baseDirectory(_ / "src")
  )

  lazy val xmlDiffProject =  Project("xml-diff-project", file("."), settings = projectSettings)

}
