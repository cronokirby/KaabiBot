import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.0"
    )),
    name := "Hello",

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.1",
      "org.pircbotx" % "pircbotx" % "2.1",
      "org.json4s" %% "json4s-native" % "3.5.0",
      "org.json4s" %% "json4s-jackson" % "3.5.0"
    )
)
