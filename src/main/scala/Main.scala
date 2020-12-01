package aoc_2020


import cats.effect._


object Main extends IOApp:

  def run(args: List[String]) =
    for
      _ <- Day1.runWithReport[IO]("inputs/input.1.txt")
    yield
      ExitCode.Success

