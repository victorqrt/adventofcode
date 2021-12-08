package aoc.twenty20


import aoc._
import aoc.Utils._


object Day1 extends Exercise:

  val day  = 1
  val year = 2020

  def process(entries: String, n: Int): Int =
    entries.split("\n")
           .map(_.toInt)
           .combinations(n)
           .find(_.sum == 2020)
           .head
           .product

  def partOne(in: String): Int = process(in, 2)

  def partTwo(in: String): Int = process(in, 3)
