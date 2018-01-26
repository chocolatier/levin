import Dependencies._

libraryDependencies += "com.regblanc" %% "scala-smtlib" % "0.2.2"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.chocolatier",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "levin",
    libraryDependencies += scalaTest % Test,
)
