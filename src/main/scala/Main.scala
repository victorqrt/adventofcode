package aoc_2020


import cats.effect._
import cats.implicits._


object Main extends IOApp:

  val exercises = List
    ( Day1 -> "inputs/input.1.txt"
    , Day2 -> "inputs/input.2.txt"
    , Day3 -> "inputs/input.3.txt"
    )

  def run(args: List[String]) =
    exercises
      .map { case (e, f) => e.runWithReport[IO](f) }
      .sequence as ExitCode.Success
