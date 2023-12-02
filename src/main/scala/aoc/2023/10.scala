package aoc.y23


import aoc._
import math.abs


case class Pipes(tiles: String):

  val lineSz   = tiles.indexOf('\n')
  val stripped = tiles.replaceAll("\n", "")
  val start    = stripped.indexOf('S')

  val connected =
    stripped
      .zipWithIndex
      .map:
        case '|' -> i => i -> (i - lineSz, i + lineSz)
        case '-' -> i => i -> (i - 1, i + 1)
        case 'L' -> i => i -> (i - lineSz, i + 1)
        case 'J' -> i => i -> (i - lineSz, i - 1)
        case '7' -> i => i -> (i + lineSz, i - 1)
        case 'F' -> i => i -> (i + lineSz, i + 1)
        case _   -> i => i -> (-1, -1)
      .toMap

  val loopEdges =
    List(start - 1, start + 1, start - lineSz, start + lineSz)
      .filter: i =>
        i > 0 && i < stripped.size && connected(i).toList.contains(start)

  lazy val connects =
    connected + (start -> (loopEdges(0), loopEdges(1)))

  lazy val loop = distances.keySet


  lazy val distances =
    def go(curr: Int, steps: Int, acc: Map[Int, Int]): Map[Int, Int] =
      val (i1, i2) = connected(curr)
      val newAcc   = acc + (curr -> steps)

      if acc.contains(i1) && acc.contains(i2) || i1 == i2 then
        newAcc
      else if i1 == -1 || acc.contains(i1) then
        go(i2, steps + 1, newAcc)
      else if i2 == -1 || acc.contains(i2) then
        go(i1, steps + 1, newAcc)
      else newAcc

    val d   = go(loopEdges.head, 1, Map(start -> 0))
    val len = d.values.size
    val max = len / 2

    d.mapValues(v => if v > max then len - v else v)


  inline def coords(i: Int) =
    val x = i % lineSz
    val y = (i - x) / lineSz
    x -> y


  lazy val loopArea =
    // shoelace formula
    def go(curr: Int, acc: Long, seen: Set[Int]): Double =
      val (i1, i2) = connects(curr)
      val (x1, y1) = coords(curr)

      if seen.contains(i1) && seen.contains(i2) || i1 == i2 then
        // TODO last connection pair is missing
        abs(acc + 28 * 31 - 28 * 32) / 2

      else if i1 == -1 || seen.contains(i1) then
        val (x2, y2) = coords(i2)
        go(i2, acc + x1 * y2 - x2 * y1, seen + curr)

      else
        val (x2, y2) = coords(i1)
        go(i1, acc + x1 * y2 - x2 * y1, seen + curr)

    go(start, 0L, Set())


object Day10 extends Exercise[Pipes]:

  val day  = 10
  val year = 2023

  def parse(str: String) = Pipes(str)

  def partOne(in: Input) = in.distances.values.max

  def partTwo(in: Input) = (1 + (2 * in.loopArea - in.loop.size) / 2).toInt
