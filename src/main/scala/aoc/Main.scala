package aoc


import cats.effect._
import cats.implicits._


object Main extends IOApp:

  def run(args: List[String]) =
    exercises
      .map { case (e, f) => e.run[IO](f) }
      .sequence as ExitCode.Success

  val exercises = List
    ( aoc.twenty20.Day1  -> "inputs/2020/1.txt"
    , aoc.twenty20.Day2  -> "inputs/2020/2.txt"
    , aoc.twenty20.Day3  -> "inputs/2020/3.txt"
    , aoc.twenty20.Day4  -> "inputs/2020/4.txt"
    , aoc.twenty20.Day5  -> "inputs/2020/5.txt"
    , aoc.twenty20.Day6  -> "inputs/2020/6.txt"
    , aoc.twenty20.Day7  -> "inputs/2020/7.txt"
    , aoc.twenty20.Day8  -> "inputs/2020/8.txt"
    , aoc.twenty20.Day9  -> "inputs/2020/9.txt"
    , aoc.twenty20.Day10 -> "inputs/2020/10.txt"

    , aoc.twenty21.Day1 -> "inputs/2021/1.txt"
    , aoc.twenty21.Day2 -> "inputs/2021/2.txt"
    , aoc.twenty21.Day3 -> "inputs/2021/3.txt"
    // , aoc.twenty21.Day4 -> "inputs/2021/4.txt"
    // , aoc.twenty21.Day5 -> "inputs/2021/5.txt"
    , aoc.twenty21.Day6 -> "inputs/2021/6.txt"
    , aoc.twenty21.Day7 -> "inputs/2021/7.txt"
    , aoc.twenty21.Day8 -> "inputs/2021/8.txt"
    )
