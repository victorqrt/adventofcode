package aoc.y21


import aoc._
import aoc.Utils._
import scala.annotation.tailrec


object Day7 extends Exercise[Array[Int]]:

  val day  = 7
  val year = 2021

  def parse(str: String) = str.strip.split(',').map(Integer.parseInt(_))

  def partOne(in: Input): Int =
    in.map(i => in.map(j => Math.abs(i - j)).sum).min
  
  def partTwo(in: Input): Int =
    in.map(i => in.map(j => Math.abs(i - j))
      .map(k => (1 + k) * k / 2).sum).min
