lazy val root = project
  .in(file("."))
  .settings(
    name                       := "aoc",
    scalaVersion               := "3.1.0",
    assembly / assemblyJarName := "aoc.jar",

    libraryDependencies ++= Seq
      ( "org.typelevel" %% "cats-effect" % "3.3.0"
      , "org.typelevel" %% "cats-core" % "2.7.0"
      , "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0"
      )
  )
