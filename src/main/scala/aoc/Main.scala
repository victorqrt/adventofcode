package aoc


import cats.effect._
import cats.implicits._


object Main extends IOApp:

  def run(args: List[String]) =
    exercises
      .map { case e -> f => e.run[IO](f) }
      .parSequence as ExitCode.Success

  val exercises = List
    ( aoc.y20.Day1  -> "inputs/2020/1.txt"
    , aoc.y20.Day2  -> "inputs/2020/2.txt"
    , aoc.y20.Day3  -> "inputs/2020/3.txt"
    , aoc.y20.Day4  -> "inputs/2020/4.txt"
    , aoc.y20.Day5  -> "inputs/2020/5.txt"
    , aoc.y20.Day6  -> "inputs/2020/6.txt"
    , aoc.y20.Day7  -> "inputs/2020/7.txt"
    , aoc.y20.Day8  -> "inputs/2020/8.txt"
    , aoc.y20.Day9  -> "inputs/2020/9.txt"
    , aoc.y20.Day10 -> "inputs/2020/10.txt"

    , aoc.y21.Day1  -> "inputs/2021/1.txt"
    , aoc.y21.Day2  -> "inputs/2021/2.txt"
    , aoc.y21.Day3  -> "inputs/2021/3.txt"
    , aoc.y21.Day4  -> "inputs/2021/4.txt"
    , aoc.y21.Day5  -> "inputs/2021/5.txt"
    , aoc.y21.Day6  -> "inputs/2021/6.txt"
    , aoc.y21.Day7  -> "inputs/2021/7.txt"
    , aoc.y21.Day8  -> "inputs/2021/8.txt"
    , aoc.y21.Day9  -> "inputs/2021/9.txt"
    , aoc.y21.Day10 -> "inputs/2021/10.txt"
    
    , aoc.y22.Day1  -> "inputs/2022/1.txt"
    , aoc.y22.Day2  -> "inputs/2022/2.txt"
    , aoc.y22.Day3  -> "inputs/2022/3.txt"
    , aoc.y22.Day4  -> "inputs/2022/4.txt"
    , aoc.y22.Day5  -> "inputs/2022/5.txt"
    , aoc.y22.Day6  -> "inputs/2022/6.txt"
    , aoc.y22.Day20 -> "inputs/2022/20.txt"
    )
