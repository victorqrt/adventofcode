package aoc.y23


import aoc._
import aoc.Utils._
import scala.math.max
import scala.util.matching.Regex._


enum Cubes:
  case RED(n: Int)
  case GREEN(n: Int)
  case BLUE(n: Int)


object Cubes:

  def apply(str: String) =
    str
      .split(",")
      .map(_.strip)
      .map:
        case s"$n red"   => RED(n.toInt)
        case s"$n green" => GREEN(n.toInt)
        case s"$n blue"  => BLUE(n.toInt)


type Game = (Int, Array[Array[Cubes]])


object Day2 extends Exercise[Array[Game]]:

  val day  = 2
  val year = 2023

  import Cubes._

  def parse(str: String) =
    str
      .split('\n')
      .flatMap: game =>
        """Game (\d+):(( \d+ \w+,?;?)+)"""
          .r
          .findAllIn(game)
          .matchData
          .map: c =>
            c.group(1).toInt -> c.group(2).split(";").map(Cubes(_))
          .toArray


  def partOne(in: Input) =
    in
      .filter(_._2.forall(_.forall:
        case RED(n)   => n <= 12
        case GREEN(n) => n <= 13
        case BLUE(n)  => n <= 14))
      .map(_._1)
      .sum


  def partTwo(in: Input) =
    in
      .map(_._2.foldLeft((0, 0, 0)):
        case (r, g, b) -> round =>
          val minR = round
            .collect:
              case RED(n) => n
            .headOption
            .getOrElse(0)

          val minG = round
            .collect:
              case GREEN(n) => n
            .headOption
            .getOrElse(0)

          val minB = round
            .collect:
              case BLUE(n) => n
            .headOption
            .getOrElse(0)

          (max(r, minR), max(g, minG), max(b, minB)))
      .map(_ * _ * _)
      .sum
