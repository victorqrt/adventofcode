package aoc_2020


import cats.effect.Sync
import cats.implicits._
import cats.Monad
import Utils._
import scala.annotation.tailrec


object Day8 extends ExerciseWithInputFile:

  type Out = String
  val day  = 8

  def run[F[_] : Monad : Sync](path: String): F[Out] =
    for
      in   <- readFile[F](path)
      prog <- M pure parse(in)
      p1   <- M pure partOne(prog)
      p2   <- M pure partTwo(prog, labelsToFlip(prog))
    yield
      s"part 1 -> $p1, part 2 -> $p2"

  enum Op:
    case Acc, Jmp, Nop

  final case class Instruction(op: Op, operand: Int, label: Int)

  def flip(i: Instruction): Instruction =
    i.copy(op = if (i.op == Op.Nop) Op.Jmp else Op.Nop)

  def labelsToFlip(is: List[Instruction]): Set[Int] =
    is.filter { case Instruction(op, _, _) => op != Op.Acc }
      .map(_._3)
      .toSet

  def strToOp(s: String): Op = s match
    case "acc" => Op.Acc
    case "jmp" => Op.Jmp
    case "nop" => Op.Nop

  def parse(src: String): List[Instruction] =
    src.split('\n')
       .zipWithIndex
       .map { case s"$op $v" -> l => Instruction(strToOp(op), v.toInt, l) }
       .toList

  def partOne(is: List[Instruction]): Int =
    eval(is, Set.empty, 0, 0)._2

  @tailrec
  def eval(is: List[Instruction], history: Set[Int], isp: Int, acc: Int):
    (Boolean, Int) =
    if (history contains isp) false -> acc
    else if (is.isEmpty || isp >= is.size) true -> acc
    else is(isp) match
      case Instruction(Op.Jmp, offset, l) =>
        eval(is, history + l, isp + offset, acc)
      case Instruction(Op.Acc, amount, l) =>
        eval(is, history + l, isp + 1, acc + amount)
      case Instruction(Op.Nop, _, l) =>
        eval(is, history + l, isp + 1, acc)

  @tailrec
  def partTwo(is: List[Instruction], toFlip: Set[Int]): Int =
    if (toFlip.isEmpty) 0
    else
      val idx = toFlip.head
      val res = eval(is.updated(idx, flip(is(idx))), Set.empty, 0, 0)
      if (res._1) res._2
      else partTwo(is, toFlip.tail)
