package aoc.y22


import aoc._
import aoc.Utils._
import scala.collection.immutable.Range.Inclusive


object Day4 extends Exercise[Array[(Inclusive, Inclusive)]]:

  val day  = 4
  val year = 2022

  def parse(str: String) =
    str.split('\n')
       .map { case s"$a-$b,$c-$d" =>
         (a.toInt to b.toInt) -> (c.toInt to d.toInt) }

  def partOne(in: Input): Int =
    in count { case (r1, r2) =>
      (r1 containsSlice r2) || (r2 containsSlice r1) }

  def partTwo(in: Input): Int =
    in count { case (r1, r2) =>
      !(r1 intersect r2).isEmpty || !(r2 intersect r1).isEmpty }
