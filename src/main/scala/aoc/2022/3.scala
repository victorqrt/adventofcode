package aoc.y22


import aoc._
import aoc.Utils._


object Day3 extends Exercise[Array[String]]:

  val day  = 3
  val year = 2022

  val letters =
    ('a'.to('z').toList ++ 'A'.to('Z').toList)
      .zipWithIndex
      .map { case (c, i) => c -> (i + 1) }
      .toMap

  def parse(str: String) = str split '\n'

  def partOne(in: Input): Int =
    in.map { case s =>
      letters(s.take(s.size / 2)
               .intersect(s.drop(s.size / 2))
               .head) }
      .sum

  def partTwo(in: Input): Int =
    in.grouped(3)
      .map { case s => letters(s.reduce(_ intersect _).head) }
      .sum
