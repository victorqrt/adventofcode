lazy val root = project
  .in(file("."))
  .settings(
    name                       := "aoc",
    scalaVersion               := "3.2.1",
    assembly / assemblyJarName := "aoc.jar",

    libraryDependencies ++= Seq
      ( "org.typelevel" %% "cats-effect" % "3.4.2"
      , "org.typelevel" %% "cats-core" % "2.9.0"
      , "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"
      )
  )
