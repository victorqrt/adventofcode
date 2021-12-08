package aoc.twenty21


import aoc._
import aoc.Utils._


object Day1 extends Exercise:

  val day  = 1
  val year = 2021

  def count(is: Seq[Int]): Int =
    is.foldLeft((-1, 0))
      { case (acc, prev) -> next => (acc + (next > prev).toInt) -> next }._1

  def partOne(in: String): Int =
    count(in.split("\n").map(_.toInt).toSeq)

  def partTwo(in: String): Int =
    count(in.split("\n").sliding(3).toSeq.map(_.map(_.toInt).sum))
