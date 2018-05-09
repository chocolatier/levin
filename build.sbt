import Dependencies._

libraryDependencies += "com.regblanc" %% "scala-smtlib" % "0.2.2"

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"

libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.3"

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.7"

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
