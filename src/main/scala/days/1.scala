package aoc_2020


import cats.effect.Sync
import cats.implicits._
import cats.Monad
import Utils._


object Day1 extends ExerciseWithInputFile:

  type Out = String
  val day  = 1

  def run[F[_] : Monad : Sync](path: String): F[Out] =
    for
      in <- readFile[F](path)
      p1 <- M pure process(in, 2)
      p2 <- M pure process(in, 3)
    yield
      s"part 1 -> $p1, part 2 -> $p2"

  def process(entries: String, n: Int): Int =
    entries.split("\n")
           .map(_.toInt)
           .combinations(n)
           .find(_.sum == 2020)
           .head
           .product
