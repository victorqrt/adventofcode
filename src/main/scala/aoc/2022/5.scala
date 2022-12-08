package aoc.y22


import aoc._
import aoc.Utils._


case class Move(from: Int, to: Int, n: Int)


object Day5 extends Exercise[(Seq[Seq[Char]], Array[Move])]:

  val day  = 5
  val year = 2022

  def parse(str: String) =
    val splitted = str split "\n\n"
    val table    = splitted.head split '\n'

    val stacks =
      (for
         i   <- 1.to(35, 4).toSeq
         row <- table
       yield
         row(i)).grouped(9).toSeq
                .map(_.dropRight(1).dropWhile(_ eq ' '))

    val moves = splitted.last.split('\n') map {
      case s"move $n from $from to $to" =>
        Move(from.toInt, to.toInt, n.toInt) }

    stacks -> moves

  def partOne(in: Input) =
    val (stacks, moves) = in
    moves.flatMap(m => List.fill(m.n)(Move(m.from, m.to, 1)))
         .foldLeft(stacks) { case (stacks, Move(from, to, _)) =>
           stacks.updated(from - 1, stacks(from - 1).tail)
                 .updated(to - 1, stacks(from - 1).head +: stacks(to - 1)) }
         .map(_.head)
         .mkString

  def partTwo(in: Input) =
    val (stacks, moves) = in
    moves.foldLeft(stacks) { case (stacks, Move(from, to, n)) =>
      stacks.updated(from - 1, stacks(from - 1).drop(n))
            .updated(to - 1, stacks(from - 1).take(n) ++ stacks(to - 1)) }
         .map(_.head)
         .mkString
