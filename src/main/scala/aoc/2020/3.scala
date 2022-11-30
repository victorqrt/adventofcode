package aoc.y20


import aoc._
import aoc.Utils._
import scala.annotation.tailrec


object Day3 extends Exercise[Array[String]]:

  val day  = 3
  val year = 2020

  val part2Slopes = List
    ( 1 -> 1
    , 3 -> 1
    , 5 -> 1
    , 7 -> 1
    , 1 -> 2
    )

  def parse(str: String) = str split "\n"

  def partOne(in: Input): Int = process(in, (3, 1))

  def partTwo(in: Input): Long =
    part2Slopes.map(process(in, _).toLong).product
  
  def process(lines: Input, slope: (Int, Int)): Int =
    val height = lines.size
    val width  = lines(0).size

    @tailrec
    def go(treeCount: Int, x: Int, y: Int): Int =

      if (y >= height) treeCount

      else
        val tc = lines(y)(x) match
          case '#' => treeCount + 1
          case _   => treeCount

        go(tc, (x + slope._1) % width, y + slope._2)

    go(0, 0, 0)
