package aoc.y21


import aoc._
import aoc.Utils._


enum Direction:
  case Forward, Down, Up

import Direction._


object Day2 extends Exercise[Array[(Direction, Int)]]:

  val day  = 2
  val year = 2021

  def parse(s: String) = s.split("\n") map {
    case s"$d $norm" => dir(d) -> norm.toInt }

  def dir(s: String): Direction = s match
    case "forward" => Forward
    case "down"    => Down
    case _         => Up

  def partOne(in: Input) =
    val r = in.foldLeft(0, 0) {
      case (len, depth) -> (Forward, n) => (len + n, depth)
      case (len, depth) -> (Down, n)    => (len, depth + n)
      case (len, depth) -> (Up, n)      => (len, depth - n)
    }

    r._1 * r._2

  def partTwo(in: Input) =
    val r = in.foldLeft(0, 0, 0) {
      case (len, depth, aim) -> (Forward, n) => (len + n, depth + n * aim, aim)
      case (len, depth, aim) -> (Down, n)    => (len, depth, aim + n)
      case (len, depth, aim) -> (Up, n)      => (len, depth, aim - n)
    }

    r._1 * r._2
