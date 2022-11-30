package aoc.y21


import aoc._
import aoc.Utils._
import scala.annotation.tailrec
import scala.collection.immutable.Set.Set3


type Grid  = Array[Array[Int]]
type Coord = (Int, Int, Int)

case class Bingo( calls:  Array[Int]
                , grids:  Array[Grid]
                , marked: List[Coord]
                , wins:   Set[Int]
                , last:   Int):

  def fulls: List[Coord] =
    marked.filter(x => 
      (marked.count(y => y._1 == x._1 && y._2 == x._2) == 5) ||
      (marked.count(y => y._1 == x._1 && y._3 == x._3) == 5))


object Day4 extends Exercise[Bingo]:

  val day  = 4
  val year = 2021

  def parse(str: String) =
    Bingo( str.split("\n").head.split(",").map(_.toInt)
         , str.split("\n").drop(1).grouped(6)
             .map(_.drop(1).map(_.strip.split("""\s+""").map(_.toInt)))
             .toArray
         , List(), Set(), 0)


  def partOne(in: Input): Int =
    val b = eval(in, _.fulls.headOption.isDefined)
    val f = b.fulls.headOption.get._1
    result(b, f)

  def partTwo(in: Input): Int =
    val b = eval(in, _b => _b.grids.size == _b.fulls.distinctBy(_._1).size)

    val f = b.fulls.last._1
    result(b, f)

  @tailrec
  def eval(b: Bingo, stop: Bingo => Boolean): Bingo =
    if stop(b) then b
    else
      val n    = b.calls.head
      val todo = b.grids
                  .zipWithIndex
                  .filter { case g -> i =>
                    !b.wins.contains(i) && g.exists(_ contains n) }

      val mark =  todo.zip(todo.map(_._1.find(_ contains n).get))
                      .map(x => ( x._1._2
                                , x._1._1 indexOf x._2
                                , x._2 indexOf n))

      eval(b.copy
        ( calls  = b.calls.tail
        , marked = b.marked ++ mark
        , wins   = b.wins ++ b.fulls.map(_._1).distinct
        , last   = n), stop)

  def result(b: Bingo, grid: Int): Int =
    b.last * b.grids(grid).flatten.filter(k =>
      b.marked.filter(_._1 == grid)
       .forall(m => k != b.grids(grid)(m._2)(m._3)))
       .sum
