package aoc_2020


import cats.effect.Sync
import cats.implicits._
import cats.Monad
import Utils._
import scala.annotation.tailrec


object Day9 extends ExerciseWithInputFile:

  type Out = String
  val day  = 9

  def run[F[_] : Monad : Sync](path: String): F[Out] =
    for
      in  <- readFile[F](path)
      arr <- M pure in.split('\n').map(_.toLong)
      p1  <- M pure partOne(arr, 1, 25)
      p2  <- M pure partTwo(arr, 1, 1, p1)
    yield
      s"part 1 -> $p1, part 2 -> $p2"

  @tailrec
  def partOne(arr: Array[Long], iter: Int, offset: Int): Long =
    val end = iter + offset
    if (end >= arr.size) 0
    else
      val sums = arr.slice(iter, end).combinations(2).map(_.sum)
      if (sums contains arr(end)) partOne(arr, iter + 1, offset)
      else arr(end)

  @tailrec
  def partTwo(arr: Array[Long], iter: Int, len: Int, res: Long): Long =
    if (iter + len >= arr.size) 0
    else
      val s   = arr.slice(iter, iter + len)
      val sum = s.sum
      if (sum == res) s.min + s.max
      else if (sum > res) partTwo(arr, iter + 1, 1, res)
      else partTwo(arr, iter, len + 1, res)
