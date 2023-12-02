package aoc.y23


import aoc._
import aoc.Utils._
import math._


case class Round(winners: Set[Int], played: Set[Int]):

  lazy val wins   = winners.count(played.contains)
  lazy val points = if wins == 0 then 0 else pow(2, wins - 1).toInt


object Day4 extends Exercise[Array[Round]]:

  val day  = 4
  val year = 2023

  def parse(str: String) =
    str
      .split('\n')
      .map:
        case s"Card $_: $w | $p" =>
          val split: String => Set[Int] =
            _.split(" ").filter(!_.isBlank).map(_.toInt).toSet

          Round(split(w), split(p))


  def partOne(in: Input) = in.map(_.points).sum

  def partTwo(in: Input) =
    in
      .view
      .map(_.wins)
      .zipWithIndex
      .foldLeft(List.fill(in.size)(1)):
        case acc -> (0 -> _) => acc
        case acc -> (w -> i) =>
          val l = min(acc.size, 1 + i + w)
          acc.take(i + 1) ++
          acc
            .drop(i + 1)
            .take(l)
            .map(_ + w) ++
          acc.drop(i + 1 + l)
      .sum + " [TODO]"
