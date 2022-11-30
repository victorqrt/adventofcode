package aoc.y20


import aoc._
import aoc.Utils._
import scala.annotation.tailrec


object Day10 extends Exercise[Array[Int]]:

  val day  = 10
  val year = 2020

  def parse(str: String) = str.split('\n').map(_.toInt).sorted

  def partOne(in: Input): Int =
    val (acc1, acc3, _) = in.foldLeft(0, 1, 0) {
      case (acc1, acc3, prev) -> next =>
        ( acc1 + (next - prev == 1).toInt
        , acc3 + (next - prev == 3).toInt
        , next ) }

    acc1 * acc3

  def partTwo(in: Input): Long = 999
