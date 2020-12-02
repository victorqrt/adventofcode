package aoc_2020


import cats.effect.Sync
import cats.implicits._
import cats.Monad
import Utils._


object Day2 extends ExerciseWithInputFile:

  type Out = String
  val day  = 2
  val re   = """(\d+)-(\d+)\s([a-z]):\s([a-z]+)""".r


  def run[F[_] : Monad : Sync](path: String): F[Out] =
    for
      in <- readFile[F](path)
      p1 <- M pure process(in, partOne)
      p2 <- M pure process(in, partTwo)
    yield
      s"part 1 -> $p1, part 2 -> $p2"

  def process(entries: String, f: String => Boolean): Int =
    entries.split("\n")
           .map(f)
           .count(_ == true)

  def partOne(entry: String): Boolean =
    entry match
      case re(min, max, l, pwd) =>
        val c = pwd count (_ == (l charAt 0))
        c >= min.toInt && c <= max.toInt

  def partTwo(entry: String): Boolean =
    entry match
      case re(idx1, idx2, l, pwd) =>
        val c  = l charAt 0
        val i1 = idx1.toInt - 1
        val i2 = idx2.toInt - 1
        ((pwd charAt i1) == c) ^ ((pwd charAt i2) == c)
