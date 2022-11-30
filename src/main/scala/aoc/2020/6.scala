package aoc.y20


import aoc._
import aoc.Utils._


object Day6 extends Exercise[Array[String]]:

  val day  = 6
  val year = 2020

  def parse(str: String) = str split "(?m)^\\s*$"

  def partOne(entries: Input): Int =
    entries.map(_.replaceAll("\n", "").toSet.size)
           .sum

  def partTwo(entries: Input): Int =
    entries.map(l => l.trim.split("\n").reduce(_ intersect _).size)
           .sum
