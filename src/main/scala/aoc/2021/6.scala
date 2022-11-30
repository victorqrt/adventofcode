package aoc.y21


import aoc._
import aoc.Utils._
import scala.annotation.tailrec


object Day6 extends Exercise[Map[Int, Long]]:

  val day  = 6
  val year = 2021

  def parse(in: String) =
    in.strip.split(',')
      .map(Integer.parseInt(_))
      .groupBy(identity)
      .view.mapValues(_.size.toLong)
      .toMap

  def partOne(in: Input): Long = eval(80, in)
  def partTwo(in: Input): Long = eval(256, in)
  
  @tailrec
  def eval(steps: Int, in: Input): Long =
    if steps == 0 then in.values.sum
    else eval(steps - 1, nextMap(in))

  def nextMap(m: Input): Map[Int, Long] =
    m.map((k, v) => if k > 0 then (k - 1, v) else 8 -> 0L) + 
      (8 -> m.getOrElse(0, 0L)) +
      (6 -> (m.getOrElse(7, 0L) + m.getOrElse(0, 0L)))
