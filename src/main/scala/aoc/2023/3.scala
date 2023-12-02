package aoc.y23


import aoc._
import aoc.Utils._
import scala.math.abs


case class Engine
  ( schema:  String
  , lineSz:  Int
  , numbers: List[List[Int]]
  , symbols: List[Int]):

  // TODO
  println(s"NUMS: $numbers")
  println(s"ADJ: ${adjacentNumbers.toList}")
  println(s"GRS: ${gears.toList}")

  def adjacent(i1: Int, i2: Int) =
    val d = abs(i1 - i2)
    d == 1 || d == lineSz

  lazy val adjacentNumbers =
    symbols
      .view
      .map: s =>
        numbers.filter: n =>
          n.find(adjacent(_, s)).isDefined
      .filter(!_.isEmpty)
      .flatten
      .map(_.map(schema).mkString.toInt)

  lazy val gears =
    symbols
      .view
      .filter(schema(_) == '*')
      .map: s =>
        numbers.filter: n =>
          n.find(adjacent(_, s)).isDefined
      .filter(_.size == 2)
      .flatten
      .map(_.map(schema).mkString.toInt)


object Day3 extends Exercise[Engine]:

  val day  = 3
  val year = 2023

  def parse(str: String) =

    val lineSz   = str.indexOf('\n')
    val stripped = str.replaceAll("\n", "")

    val (digits, symbols) = stripped
      .zipWithIndex
      .foldLeft(List[Int]() -> List[Int]()):
        case (is -> syms) -> (c -> i) if c.isDigit => (is :+ i) -> syms
        case (is -> syms) -> (c -> i) if c != '.'  => is -> (syms :+ i)
        case (is -> syms) -> _                     => is -> syms

    val numbers = digits
      .foldLeft(List[List[Int]]() -> -1):
        case acc -> prev -> next if next != prev + 1 =>
          (acc :+ List(next)) -> next
        case acc -> _ -> next =>
          (acc.dropRight(1) :+ (acc.last :+ next)) -> next

    Engine(stripped, lineSz, numbers._1, symbols)


  def partOne(in: Input) =
    in.adjacentNumbers.sum

  def partTwo(in: Input) =
    in.gears
      .grouped(2)
      .map(_.product)
      .sum
