package aoc.y22


import aoc._
import aoc.Utils._


object Day2 extends Exercise[Array[String]]:

  val day  = 2
  val year = 2022

  val win    = Map("A" -> "Y", "B" -> "Z", "C" -> "X")
  val lose   = Map("A" -> "Z", "B" -> "X", "C" -> "Y")
  val draw   = Map("A" -> "X", "B" -> "Y", "C" -> "Z")
  val scores = Map("X" -> 1,   "Y" -> 2,     "Z" -> 3)

  def parse(str: String) = str split '\n'

  def partOne(in: Input): Int =
    in.map { 
      case s"$fst $snd" =>
        if win(fst) == snd then scores(snd) + 6
        else if draw(fst) == snd then scores(snd) + 3
        else scores(snd) }
      .sum

  def partTwo(in: Input): Int =
    in.map {
      case s"$fst X" => scores(lose(fst))
      case s"$fst Y" => 3 + scores(draw(fst))
      case s"$fst Z" => 6 + scores(win(fst)) }
      .sum
