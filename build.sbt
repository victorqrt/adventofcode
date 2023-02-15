lazy val root = project
  .in(file("."))
  .settings(
    name                       := "aoc",
    scalaVersion               := "3.2.2",
    assembly / assemblyJarName := "aoc.jar",

    libraryDependencies ++= Seq
      ( "org.typelevel" %% "cats-effect" % "3.4.6"
      , "org.typelevel" %% "cats-core" % "2.9.0"
      )
  )
