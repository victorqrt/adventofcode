package aoc.twenty21


import aoc._
import aoc.Utils._


object Day2 extends Exercise:

  val day  = 2
  val year = 2021

  enum Direction:
    case Forward, Down, Up

  import Direction._

  def dir(s: String): Direction = s match
    case "forward" => Forward
    case "down"    => Down
    case _         => Up

  def parse(s: String): Array[(Direction, Int)] =
    s.split("\n").map { case s"$d $norm" => dir(d) -> norm.toInt }

  def partOne(in: String) =
    val r = parse(in).foldLeft(0, 0) {
      case (len, depth) -> (Forward, n) => (len + n, depth)
      case (len, depth) -> (Down, n)    => (len, depth + n)
      case (len, depth) -> (Up, n)      => (len, depth - n)
    }

    r._1 * r._2

  def partTwo(in: String) =
    val r = parse(in).foldLeft(0, 0, 0) {
      case (len, depth, aim) -> (Forward, n) => (len + n, depth + n * aim, aim)
      case (len, depth, aim) -> (Down, n)    => (len, depth, aim + n)
      case (len, depth, aim) -> (Up, n)      => (len, depth, aim - n)
    }

    r._1 * r._2
