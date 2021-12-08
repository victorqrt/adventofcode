package aoc.twenty20


import aoc._
import aoc.Utils._
import scala.annotation.tailrec


object Day9 extends Exercise:

  val day  = 9
  val year = 2020

  def partOne(in: String): Long =

    @tailrec
    def go(arr: Array[Long], i: Int, offset: Int): Long =
      val end = i + offset
      if (end >= arr.size) 0
      else
        val sums = arr.slice(i, end).combinations(2).map(_.sum)
        if (sums contains arr(end)) go(arr, i + 1, offset)
        else arr(end)

    go(in.split('\n').map(_.toLong), 1, 25)

  def partTwo(in: String): Long =

    @tailrec
    def go(arr: Array[Long], i: Int, len: Int, res: Long): Long =
      if (i + len >= arr.size) 0
      else
        val s   = arr.slice(i, i + len)
        val sum = s.sum
        if (sum == res) s.min + s.max
        else if (sum > res) go(arr, i + 1, 1, res)
        else go(arr, i, len + 1, res)

    go(in.split('\n').map(_.toLong), 1, 1, partOne(in))
