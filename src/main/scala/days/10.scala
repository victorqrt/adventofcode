package aoc_2020


import cats.effect.Sync
import cats.implicits._
import cats.Monad
import Utils._
import scala.annotation.tailrec


object Day10 extends ExerciseWithInputFile:

  type Out = String
  val day  = 10

  def run[F[_] : Monad : Sync](path: String): F[Out] =
    for
      in  <- readFile[F](path)
      arr <- M pure in.split("\n").map(_.toInt)
      p1  <- M pure partOne(arr)
    yield
      s"part 1 -> $p1, part 2 -> p2"

  def partOne(in: Array[Int]): Int =
    val (acc1, acc3, _) =
     in.sorted.foldLeft(0, 1, 0) {
         case (acc1, acc3, prev) -> next =>
           ( acc1 + (next - prev == 1).toInt
           , acc3 + (next - prev == 3).toInt
           , next
           )
       }

    acc1 * acc3

  def partTwo(in: Array[Int]): Long =

    def validateReverseSorted(arr: Array[Int]): Boolean =
      ((in.max + 3) +: arr).foldLeft(true -> 0) {
        case (acc, prev) -> next => (acc && (prev - next <= 3)) -> next
      }._1

    val ord = in.sorted.reverse

    (1 to ord.size)
      .flatMap(ord.combinations)
      .filter(validateReverseSorted)
      .size
