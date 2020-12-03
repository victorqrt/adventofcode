package aoc_2020


import cats.effect.Sync
import cats.implicits._
import cats.Monad
import Utils._
import scala.annotation.tailrec


object Day3 extends ExerciseWithInputFile:

  type Out = String
  val day  = 3

  val part2Slopes = List
    ( 1 -> 1
    , 3 -> 1
    , 5 -> 1
    , 7 -> 1
    , 1 -> 2
    )

  def run[F[_] : Monad : Sync](path: String): F[Out] =
    for
      in <- readFile[F](path)
      p1 <- M pure process(in, (3, 1))
      p2 <- M pure part2Slopes.map(process(in, _).toLong).product
    yield
      s"part 1 -> $p1, part 2 -> $p2"

  def process(entries: String, slope: (Int, Int)): Int =

    val lines  = entries.split("\n")
    val height = lines.size
    val width  = lines(0).size

    @tailrec
    def go(treeCount: Int, x: Int, y: Int): Int =

      if (y >= height) treeCount

      else
        val tc = lines(y)(x) match
          case '#' => treeCount + 1
          case _   => treeCount

        go(tc, (x + slope._1) % width, y + slope._2)

    go(0, 0, 0)
