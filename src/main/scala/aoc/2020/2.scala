package aoc.twenty20


import aoc._
import aoc.Utils._


object Day2 extends Exercise:
  val day  = 2
  val year = 2020
  val re   = """(\d+)-(\d+)\s([a-z]):\s([a-z]+)""".r

  def process(entries: String, f: String => Boolean): Int =
    entries.split("\n")
           .map(f)
           .count(_ == true)

  def one(entry: String): Boolean =
    entry match
      case re(min, max, l, pwd) =>
        (pwd count (_ == (l charAt 0))).between(min.toInt, max.toInt)
      case _ => false

  def two(entry: String): Boolean =
    entry match
      case re(idx1, idx2, l, pwd) =>
        val c  = l charAt 0
        val i1 = idx1.toInt - 1
        val i2 = idx2.toInt - 1
        ((pwd charAt i1) == c) ^ ((pwd charAt i2) == c)
      case _ => false

  def partOne(in: String) = process(in, one)

  def partTwo(in: String) = process(in, two)
