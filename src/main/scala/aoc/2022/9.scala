package aoc.y22


import aoc._
import aoc.Utils._
import math._
import scala.annotation.tailrec


object Day9 extends Exercise[Array[String]]:

  val day  = 9
  val year = 2022

  type Coord = (Int, Int)

  case class Knot(head: Coord, tail: Coord, visited: Set[Coord]):

    def adjacent = abs(head._1 - tail._1) <= 1 && abs(head._2 - tail._2) <= 1

    def putHead(c: Coord) = this.copy(head = c)

    def stepHead(dx: Int = 0, dy: Int = 0) =
      putHead((head._1 + dx, head._2 + dy))

    def stepTail =
      val newTail =
        ( tail._1 + signum(head._1 - tail._1)
        , tail._2 + signum(head._2 - tail._2))
      this.copy(tail = newTail, visited = visited + newTail)

    def dragTail: Knot = // not stack safe but <= |head - tail|
      if adjacent then this
      else stepTail.dragTail


  val start = Knot(0 -> 0, 0 -> 0, Set(0 -> 0))

  def parse(str: String) = str split '\n'

  def partOne(in: Input) =
    in.foldLeft(start) {
      case k -> s"U $n" => k.stepHead(dy = n.toInt).dragTail
      case k -> s"D $n" => k.stepHead(dy = -n.toInt).dragTail
      case k -> s"L $n" => k.stepHead(dx = -n.toInt).dragTail
      case k -> s"R $n" => k.stepHead(dx = n.toInt).dragTail
      case k -> _       => k }
      .visited
      .size


  def partTwo(in: Input) =

    @tailrec
    def update( n: Int, dx: Int, dy: Int
              , ks: List[Knot], acc: List[Knot] = Nil): List[Knot] =
      if n == 0 then ks
      else if ks.isEmpty then update(n - 1, dx, dy, acc)
      else
        val h = 
          if acc.isEmpty then ks.head.stepHead(dx, dy).dragTail
          else ks.head.dragTail

        val t = 
          if ks.tail.isEmpty then Nil
          else ks.tail.head.putHead(h.tail) +: ks.tail.tail
        
        update(n, dx, dy, t, acc :+ h)

    in.foldLeft(List.fill(9)(start)) {
      case ks -> s"U $n" => update(n.toInt, 0, 1, ks)
      case ks -> s"D $n" => update(n.toInt, 0, -1, ks)
      case ks -> s"L $n" => update(n.toInt, -1, 0, ks)
      case ks -> s"R $n" => update(n.toInt, 1, 0, ks)
      case ks -> _       => ks }
      .last
      .visited
      .size
