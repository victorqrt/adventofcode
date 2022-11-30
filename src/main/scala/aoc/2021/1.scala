package aoc.y21


import aoc._
import aoc.Utils._


object Day1 extends Exercise[Array[Int]]:

  val day  = 1
  val year = 2021

  def parse(str: String) = str.split('\n').map(_.toInt)

  def partOne(in: Input): Int = count(in.toSeq)

  def partTwo(in: Input): Int = count(in.sliding(3).toSeq.map(_.sum))
  def count(is: Seq[Int]): Int =
    is.foldLeft((-1, 0))
      { case (acc, prev) -> next => (acc + (next > prev).toInt) -> next }._1
