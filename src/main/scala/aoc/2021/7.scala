package aoc.twenty21


import aoc._
import aoc.Utils._
import scala.annotation.tailrec


object Day7 extends Exercise:

  val day  = 7
  val year = 2021

  def partOne(in: String): Int =
    val is = in.strip.split(",").map(Integer.parseInt(_))
    is.map(i => is.map(j => Math.abs(i - j)).sum).min
  
  def partTwo(in: String): Int =
    val is = in.strip.split(",").map(Integer.parseInt(_))
    is.map(i => is.map(j => Math.abs(i - j))
      .map(k => (1 + k) * k / 2).sum).min
