package aoc.y21


import aoc._
import aoc.Utils._
import scala.annotation.tailrec


object Day9 extends Exercise[Seq[Seq[Int]]]:

  val day  = 9
  val year = 2021

  def parse(in: String) =
    in.split('\n').map(_.map(_.asDigit).filter(_ != -1))

  def partOne(mtx: Input): Int =
    mtx.flatten
       .zip(lows(mtx))
       .filter(_._2)
       .map(1 + _._1)
       .sum

  def partTwo(mtx: Input): Int =
    @tailrec
    def go(todo: Set[(Int, Int)], done: Set[(Int, Int)], basinSz: Int): Int =
      if todo.isEmpty then basinSz
      else
        val (x, y) = todo.head
        val next   = adj(mtx, x, y) filter { case _x -> _y =>
          !done.contains(_x -> _y) && mtx(_y)(_x).between(mtx(y)(x), 8) }

        go(todo.tail ++ next, done + (x -> y), basinSz + 1)

    val coords =
      for
        y <- 0 until mtx.size
        x <- 0 until mtx.head.size
      yield
        x -> y

    mtx.flatten
       .zip(lows(mtx))
       .zip(coords)
       .filter(_._1._2)
       .map(x => go(Set(x._2), Set(), 0))
       .sorted(Ordering.Int.reverse)
       .take(3)
       .product

  def lows(mtx: Input): Seq[Boolean] =
    for
      y <- 0 until mtx.size
      x <- 0 until mtx.head.size
    yield
      adj(mtx, x, y) forall {
        case _x -> _y => mtx(_y)(_x) > mtx(y)(x) }

  def adj(mtx: Input, x: Int, y: Int): Seq[(Int, Int)] =
    Seq((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)) filter {
      case _x -> _y =>
        _x.between(0, mtx.head.size - 1) && _y.between(0, mtx.size - 1) }
