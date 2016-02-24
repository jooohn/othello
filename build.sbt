name := "othello"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.2"

lazy val root = (project in file(".")).
  settings(
    name := "othello",
    version := "1.0",
    scalaVersion := "2.11.5"
  )
