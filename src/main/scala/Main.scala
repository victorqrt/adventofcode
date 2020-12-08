package aoc_2020


import cats.effect._
import cats.implicits._


object Main extends IOApp:

  def run(args: List[String]) =
    exercises
      .map { case (e, f) => e.runWithReport[IO](f) }
      .sequence as ExitCode.Success

  val exercises = List
    ( Day1 -> "inputs/input.1.txt"
    , Day2 -> "inputs/input.2.txt"
    , Day3 -> "inputs/input.3.txt"
    , Day4 -> "inputs/input.4.txt"
    , Day5 -> "inputs/input.5.txt"
    , Day6 -> "inputs/input.6.txt"
    , Day7 -> "inputs/input.7.txt"
    , Day8 -> "inputs/input.8.txt"
    )
