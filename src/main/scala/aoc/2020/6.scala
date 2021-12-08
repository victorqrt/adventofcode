package aoc.twenty20


import aoc._
import aoc.Utils._


object Day6 extends Exercise:

  val day  = 6
  val year = 2020

  def partOne(entries: String): Int =
    entries.split("(?m)^\\s*$").map(_.replaceAll("\n", "").toSet.size).sum

  def partTwo(entries: String): Int =
    entries.split("(?m)^\\s*$")
           .map(l => l.trim.split("\n").reduce(_ intersect _).size).sum
