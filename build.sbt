import Dependencies._

libraryDependencies += "com.regblanc" %% "scala-smtlib" % "0.2.2"

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"

libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.3"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.7"

libraryDependencies += "com.github.pureconfig" %% "pureconfig" % "0.9.1"

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
