organization := "com.scryetek"

name := "gl-utils"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.7"

enablePlugins(ScalaJSPlugin)

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.8.2",
  "com.scryetek" %%% "vecmath" % "0.3.1"
)
