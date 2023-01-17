package aoc.y22


import aoc._
import aoc.Utils._
import math._


object Day10 extends Exercise[List[Int]]:

  enum Instruction:
    case Noop
    case Add(n: Int)

  import Instruction._

  val day  = 10
  val year = 2022

  def parse(str: String) =
    str.split('\n').map {
      case "noop"     => Noop
      case s"addx $n" => Add(n.toInt) }
       .foldLeft(List(1)) {
      case is -> Noop   => is :+ is.last
      case is -> Add(n) =>
        is ++ List(is.last, is.last + n) }

  def partOne(in: Input) =
    List(20, 60, 100, 140, 180, 220)
      .map(k => k * in(k - 1))
      .sum

  def partTwo(in: Input) = '\n' +
    in.tail.grouped(40).map { case row =>
      row.foldLeft(1, "") { case (idx, str) -> next =>
        (idx + 1, str + (if abs(idx - next) > 1 then '.' else '#')) } }
      .map(_._2.mkString).mkString("\n")
