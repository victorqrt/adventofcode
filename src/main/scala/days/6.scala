package aoc_2020


import cats.effect.Sync
import cats.implicits._
import cats.Monad
import Utils._


object Day6 extends ExerciseWithInputFile:

  type Out = String
  val day  = 6

  def run[F[_] : Monad : Sync](path: String): F[Out] =
    for
      in <- readFile[F](path)
      p1 <- M pure partOne(in)
      p2 <- M pure partTwo(in)
    yield
      s"part 1 -> $p1, part 2 -> $p2"

  def partOne(entries: String): Int =
    entries.split("(?m)^\\s*$")
           .map(_.replaceAll("\n", "").toSet.size)
           .sum

  def partTwo(entries: String): Int =
    entries.split("(?m)^\\s*$")
           .map(l => l.trim.split("\n").reduce(_ intersect _).size)
           .sum
