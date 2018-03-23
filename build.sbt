import Dependencies._

libraryDependencies += "com.regblanc" %% "scala-smtlib" % "0.2.2"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5" 
libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test"

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
