package aoc.twenty21


import aoc._
import aoc.Utils._
import scala.annotation.tailrec


object Day8 extends Exercise:

  val day  = 8
  val year = 2021

  def partOne(in: String): Int =
    in.split("\n")
      .flatMap(_.split("""\s+\|\s+""").tail.flatMap(_ split " "))
      .count(Array(2, 3, 4, 7) contains _.size)

  def partTwo(in: String): Int =

    val lines = in.split("\n").map(_.split("""\s+\|\s+""").map(_ split " "))
    val sigs  = lines.map(_(0).map(_.sorted))
    val dgts  = lines.map(_(1).map(_.sorted))

    @tailrec
    def go(hints: Array[String], m: Map[String, Int]): Map[String, Int] =
      if hints.isEmpty then m
      else
        val str  = hints.head
        val newM = str.size match
          case 2 => m + (str -> 1)
          case 3 => m + (str -> 7)
          case 4 => m + (str -> 4)
          case 7 => m + (str -> 8)
          case 5 => if m.keyOf(1) inside str then m + (str -> 3)
                    else if str.diff(m.keyOf(4)).size == 3 then m + (str -> 2)
                    else m + (str -> 5)
          case 6 => if m.keyOf(4) inside str then m + (str -> 9)
                    else if m.keyOf(1) inside str then m + (str -> 0)
                    else m + (str -> 6)
          case _ => m

        go(hints.tail, newM)

    val mappings = sigs.map(s => go(s.sortBy(_.size), Map()))

    dgts.zip(mappings)
        .map {
          case Array(d1, d2, d3, d4) -> m =>
            1000 * m(d1) + 100 * m(d2) + 10 * m(d3) + m(d4) }
        .sum
