package aoc.twenty20


import aoc._
import aoc.Utils._
import scala.annotation.tailrec


object Day10 extends Exercise:

  val day  = 10
  val year = 2020

  def partOne(in: String): Int =
    val (acc1, acc3, _) =
     in.split("\n")
       .map(_.toInt).sorted
       .foldLeft(0, 1, 0) {
         case (acc1, acc3, prev) -> next =>
           ( acc1 + (next - prev == 1).toInt
           , acc3 + (next - prev == 3).toInt
           , next )
       }

    acc1 * acc3

  def partTwo(in: String): Long = 999
