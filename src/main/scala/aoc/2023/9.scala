package aoc.y23


import aoc._


type AALong = Array[Array[Long]]

object Day9 extends Exercise[Array[AALong]]:

  val day  = 9
  val year = 2023

  def parse(str: String) =

    def diffs(is: Array[Long]) =
      is.sliding(2)
        .map:
          case Array(i, j) => j - i
        .toArray

    def generateDiffSeqs(is: Array[Long], acc: AALong): AALong =
      if is.filter(_ != 0).isEmpty then acc
      else generateDiffSeqs(diffs(is), acc :+ is)

    str
      .split("\n")
      .map(_.split(" ").map(_.toLong))
      .map(generateDiffSeqs(_, Array()))


  def partOne(in: Input) =
    in.map(_.map(_.last).sum).sum


  def partTwo(in: Input) =
    - in
      .map(_.map(_.head).reverse)
      .map(_.foldLeft(0L)(- _ - _))
      .sum
