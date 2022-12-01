package aoc.y22


import aoc._
import aoc.Utils._


object Day1 extends Exercise[Array[Int]]:

  val day  = 1
  val year = 2022

  def parse(str: String) =
    str.split("\n\n")
       .map(_.split('\n').map(_.toInt).sum)
       .sorted(Ordering.Int.reverse)

  def partOne(in: Input): Int = in.max

  def partTwo(in: Input): Int = in.take(3).sum
