package aoc.y20


import aoc._
import aoc.Utils._


object Day2 extends Exercise[Array[String]]:
  val day  = 2
  val year = 2020
  val re   = """(\d+)-(\d+)\s([a-z]):\s([a-z]+)""".r

  def parse(str: String) = str split "\n"
  
  def partOne(in: Input) = process(in, one)
  def partTwo(in: Input) = process(in, two)

  def process(entries: Input, f: String => Boolean): Int =
    entries.map(f).count(_ == true)

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
