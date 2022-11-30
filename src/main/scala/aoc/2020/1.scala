package aoc.y20


import aoc._
import aoc.Utils._


object Day1 extends Exercise[Array[Int]]:

  val day  = 1
  val year = 2020

  def parse(str: String) = str.split("\n").map(_.toInt)
  
  def partOne(in: Input): Int = process(in, 2)
  def partTwo(in: Input): Int = process(in, 3)

  def process(in: Input, n: Int): Int =
    in.combinations(n)
      .find(_.sum == 2020)
      .head
      .product
