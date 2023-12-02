package aoc.y23


import aoc._
import aoc.Utils._
import math._


object Day6 extends Exercise[Seq[(Long, Long)]]:

  val day  = 6
  val year = 2023

  def parse(str: String) =
    val split: String => Seq[Long] =
      _.split(":").last.split(" ").filter(!_.isBlank).map(_.strip.toLong)

    val lines = str.split("\n")
    split(lines.head).zip(split(lines.last))


  def waysToWin(time: Long, record: Long) =
    val delta = sqrt(pow(time, 2) - 4 * record)
    val lower = ((time - delta) / 2 + 1).floor.toLong
    val upper = ((time + delta) / 2 - 1).ceil.toLong
    upper - lower + 1


  def partOne(in: Input) = in.map(waysToWin).product


  def partTwo(in: Input) =
    val (times, distances) = in.unzip
    waysToWin(times.mkString.toLong, distances.mkString.toLong)
