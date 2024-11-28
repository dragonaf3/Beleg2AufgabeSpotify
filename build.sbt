ThisBuild / version := "1.0.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "Beleg2AufgabeSpotify",
    scalaVersion := "3.5.1",
    Compile / scalacOptions ++= Seq("-deprecation"),
    Compile / console / scalacOptions --= Seq("-Ywarn-unused", "-Ywarn-unused-import"),
    Test / fork := true,
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.17.1",
      "org.scalactic" %% "scalactic" % "3.2.19" % "test",
      "org.scalatest" %% "scalatest" % "3.2.19" % "test")

  )
  
