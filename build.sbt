name := "error_messages"

enablePlugins(GitVersioning)

git.useGitDescribe := true

version := "0.1"

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.11.8", "2.12.1")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature"
)


val error_messages = Project(id = "error_messages", base = file("."))