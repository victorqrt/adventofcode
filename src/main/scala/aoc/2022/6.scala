package aoc.y22


import aoc._
import aoc.Utils._


object Day6 extends Exercise[String]:

  val day  = 6
  val year = 2022

  def parse(str: String) = str

  def partOne(in: Input) = find(in, 4)

  def partTwo(in: Input) = find(in, 14)

  def find(in: Input, len: Int) =
    in.sliding(len).zipWithIndex.find(_._1.toSet.size == len).get._2 + len
