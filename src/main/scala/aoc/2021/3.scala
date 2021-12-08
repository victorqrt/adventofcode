package aoc.twenty21


import aoc._
import aoc.Utils._
import scala.annotation.tailrec


object Day3 extends Exercise:

  val day  = 3
  val year = 2021

  def partOne(in: String): Int =
    val arr     = in.split("\n")
    val epsilon = arr.head
      .zipWithIndex
      .map((_, i) => {
        val is   = arr.map(_(i))
        val ones = is.count(_ equals '1')
        (ones >= is.size - ones).toInt })
      .mkString

    val gamma = epsilon.negateBits

    Integer.parseInt(epsilon, 2) * Integer.parseInt(gamma, 2)

  def partTwo(in: String): Int =
    val arr = in.split("\n")

    @tailrec
    def go(_arr: Array[String], i: Int, cmp: (Int, Int) => Boolean): Int =
      if _arr.size > 1 then
        val bits = _arr.map(_(i))
        val ones = bits.count(_ equals '1')
        val next =
          if cmp(ones, bits.size - ones) then '1'
          else '0'

        go(_arr.filter(_(i) equals next), i + 1, cmp)

      else Integer.parseInt(_arr.head, 2)

    go(arr, 0, _ >= _) * go(arr, 0, _ < _)
