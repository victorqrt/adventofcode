package aoc_2020


import cats.effect._
import cats.implicits._
import cats.Monad
import Utils._


object Day1 extends Exercise[String, String]:

  val day = 1

  def run[F[_] : Effect : LiftIO : Monad](path: String): F[String] =
    for
      input <- readFile[F](path)
      p1    <- IO(process(input, 2)).to[F]
      p2    <- IO(process(input, 3)).to[F]
    yield
      s"part 1 -> $p1 part 2 -> $p2"

  def process(entries: String, n: Int): Int =
    entries.split("\n")
           .combinations(n)
           .map  { case xs => xs.map(_.toInt) }
           .find { case xs => xs.sum == 2020 }
           .head
           .reduce(_ * _)
