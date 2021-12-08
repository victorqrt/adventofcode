package aoc.twenty20


import aoc._
import aoc.Utils._


object Day5 extends Exercise:

  val day  = 5
  val year = 2020

  def partition(c: Char, min: Int, max: Int): (Int, Int) =
    val m = (min + max) / 2
    c match
      case 'F' | 'L' => (min, m)
      case 'B' | 'R' => (m + 1, max)

  def row(str: String): Int =
    str.foldLeft(0, 127) { case (acc, next) => partition(next, acc._1, acc._2) }._1

  def col(str: String): Int =
    str.foldLeft(0, 7) { case (acc, next) => partition(next, acc._1, acc._2) }._1

  def seatNumber(str: String): Int =
    8 * row(str.substring(0, 7)) + col(str.substring(7))

  def partOne(entries: String): Int =
    entries.split("\n").map(seatNumber).max

  def partTwo(entries: String): Int =
    val seats = entries.split("\n") map seatNumber

    (0 to 127 * 8).filter((n: Int) =>
      !(seats contains n) && (seats contains n - 1) && (seats contains n + 1))
                  .head
