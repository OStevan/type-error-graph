name := "error_messages"

enablePlugins(GitVersioning)

git.useGitDescribe := true

version := "0.1"

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.11.8", "2.12.1")

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature"
)


lazy val inox = RootProject(file("../inox"))

val error_messages = Project(id = "error_messages", base = file(".")).dependsOn(inox)