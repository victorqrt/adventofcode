package aoc.twenty21


import aoc._
import aoc.Utils._
import scala.annotation.tailrec


object Day6 extends Exercise:

  val day  = 6
  val year = 2021

  def parse(in: String): Map[Int, Long] =
    in.strip.split(",")
      .map(Integer.parseInt(_))
      .groupBy(identity)
      .view.mapValues(_.size.toLong)
      .toMap

  def nextMap(m: Map[Int, Long]): Map[Int, Long] =
    m.map((k, v) => if k > 0 then (k - 1, v) else 8 -> 0L) + 
      (8 -> m.getOrElse(0, 0L)) +
      (6 -> (m.getOrElse(7, 0L) + m.getOrElse(0, 0L)))

  @tailrec
  def eval(steps: Int, in: Map[Int, Long]): Long =
    if steps == 0 then in.values.sum
    else eval(steps - 1, nextMap(in))

  def partOne(in: String): Long = eval(80, parse(in))
  
  def partTwo(in: String): Long = eval(256, parse(in))
