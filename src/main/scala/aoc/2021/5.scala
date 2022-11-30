package aoc.y21


import aoc._
import aoc.Utils._
import scala.annotation.tailrec


type Point = (Int, Int)
type Line  = (Point, Point)

object Day5 extends Exercise[Seq[Line]]:

  val day  = 5
  val year = 2021

  def parse(str: String) = str.split('\n') map {
    case s"$x1,$y1 -> $x2,$y2" =>
      (x1.toInt, y1.toInt) -> (x2.toInt, y2.toInt) }

  def partOne(in: Input): Int =
    count(in filter { case l =>
      l._1._1 == l._2._1 || l._1._2 == l._2._2 })

  def partTwo(in: Input): Int = count(in)
  
  def count(ps: Seq[Line]): Int =
    ps.flatMap(pointsIn)
      .groupBy(identity)
      .mapValues(_.size)
      .count { case k -> v => v > 1 }

  def pointsIn(line: Line): Seq[Point] =

    val dx  = line._2._1 compare line._1._1
    val dy  = line._2._2 compare line._1._2

    0.to(math.abs(line._1._1 - line._2._1)
      .max(math.abs(line._1._2 - line._2._2)))
      .map(n => (n * dx + line._1._1, n * dy + line._1._2))
