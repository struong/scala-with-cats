name := "scala-with-cats"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.8"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"
// scalac options come from the sbt-tpolecat plugin so need to set any here

ThisBuild / scalacOptions += "-P:kind-projector:underscore-placeholders"
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)
