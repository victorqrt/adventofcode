package aoc.twenty20


import aoc._
import aoc.Utils._
import scala.annotation.tailrec


object Day8 extends Exercise:

  val day  = 8
  val year = 2020

  enum Op:
    case Acc, Jmp, Nop

  final case class Instruction(op: Op, operand: Int, label: Int)

  def flip(i: Instruction): Instruction =
    i.copy(op = if (i.op == Op.Nop) Op.Jmp else Op.Nop)

  def strToOp(s: String): Op = s match
    case "acc" => Op.Acc
    case "jmp" => Op.Jmp
    case "nop" => Op.Nop

  def parse(src: String): List[Instruction] =
    src.split('\n')
       .zipWithIndex
       .map { case s"$op $v" -> l => Instruction(strToOp(op), v.toInt, l) }
       .toList

  @tailrec
  def eval(is: List[Instruction], history: Set[Int], isp: Int, acc: Int):
    (Boolean, Int) =
    if (history contains isp) false -> acc
    else if (isp >= is.size) true -> acc
    else is(isp) match
      case Instruction(Op.Jmp, offset, l) =>
        eval(is, history + l, isp + offset, acc)
      case Instruction(Op.Acc, amount, l) =>
        eval(is, history + l, isp + 1, acc + amount)
      case Instruction(Op.Nop, _, l) =>
        eval(is, history + l, isp + 1, acc)

  def partOne(in: String): Int = eval(parse(in), Set.empty, 0, 0)._2

  def partTwo(in: String): Int =
    val is = parse(in)
    is.filter { case Instruction(op, _, _) => op != Op.Acc }
      .toSet
      .map(i => eval(is.updated(i.label, flip(i)), Set.empty, 0, 0))
      .find(_._1)
      .map(_._2)
      .get
