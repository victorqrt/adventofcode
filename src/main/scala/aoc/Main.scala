package aoc


import cats.effect._
import cats.effect.std._
import cats.implicits._
import Utils._


object Main extends IOApp:

  def run(args: List[String]) =
    for
      _      <- Console[IO]
                  .println("🎅 Ho ho ho...")

      start  <- Clock[IO].monotonic
      result <- exercises
                  .map:
                    case day -> fd =>
                      readFile[IO](fd).flatMap(day.run[IO])
                  .parSequence

      finish <- Clock[IO].monotonic
      _      <- result
                  .map(Console[IO].println)
                  .sequence

      time    = (finish - start).toMillis / 1000.0
      _      <- Console[IO]
                  .println(s"\n🎄🏁 ${time}s ($cpus cores) 🏁🎄")
    yield
      ExitCode.Success


  val exercises = List
    ( aoc.y20.Day1  -> "inputs/2020/1.txt"/*
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
    , aoc.y22.Day7  -> "inputs/2022/7.txt"
    , aoc.y22.Day9  -> "inputs/2022/9.txt"
    , aoc.y22.Day10 -> "inputs/2022/10.txt"
    //, aoc.y22.Day11 -> "inputs/2022/11.txt"
    , aoc.y22.Day20 -> "inputs/2022/20.txt"*/

    , aoc.y23.Day1  -> "inputs/2023/1.txt"
    , aoc.y23.Day2  -> "inputs/2023/2.txt"
    //, aoc.y23.Day3  -> "inputs/2023/3.txt"
    , aoc.y23.Day4  -> "inputs/2023/4.txt"
    , aoc.y23.Day5  -> "inputs/2023/5.txt"
    , aoc.y23.Day6  -> "inputs/2023/6.txt"
    , aoc.y23.Day8  -> "inputs/2023/8.txt"
    , aoc.y23.Day9  -> "inputs/2023/9.txt"
    , aoc.y23.Day10 -> "inputs/2023/10.txt"
    )
