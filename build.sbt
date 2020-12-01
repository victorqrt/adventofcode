val dottyVersion = "3.0.0-M2"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "aoc_2020",
    scalaVersion := dottyVersion,

    assemblyJarName in assembly := "aoc_2020.jar",

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "2.3.0",
      "org.typelevel" %% "cats-core" % "2.3.0"
    )
  )
