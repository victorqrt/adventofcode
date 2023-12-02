package aoc.y23


import aoc._
import aoc.Utils._


object Day1 extends Exercise[Array[String]]:

  val day  = 1
  val year = 2023

  def parse(str: String) = str.split('\n')


  def partOne(in: Input) =
    in.map:
        case str =>
          val digits = str.toCharArray.filter(_.isDigit)
          s"${digits.head}${digits.last}".toInt
      .sum


  def partTwo(in: Input): Int =
    in.map:
        case str =>
          val digits = scanForDigits(str)
          s"${digits.head}${digits.last}".toInt
      .sum


  val digits = Map
    ( "one"   -> "1"
    , "two"   -> "2"
    , "three" -> "3"
    , "four"  -> "4"
    , "five"  -> "5"
    , "six"   -> "6"
    , "seven" -> "7"
    , "eight" -> "8"
    , "nine"  -> "9"
    )

  val search = digits.keySet ++ digits.values.toSet

  def scanForDigits(str: String) =
    search
      .filter(_.length <= str.length)
      .foldLeft(List[(Int, String)]()):
        case acc -> toFind =>
          acc ++
          str
            .sliding(toFind.length)
            .zipWithIndex
            .filter(_._1 == toFind)
            .map(_.swap)
      .toList
      .view
      .sortBy(_._1)
      .map:
        case _ -> d => if d.head.isDigit then d else digits(d)
