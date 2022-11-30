package aoc.y20


import aoc._
import aoc.Utils._
import scala.annotation.tailrec


enum Op:
  case Acc, Jmp, Nop

import Op._

case class Instruction(op: Op, operand: Int, label: Int):
  def flip = this.copy(op = if op == Nop then Jmp else Nop)


object Day8 extends Exercise[List[Instruction]]:

  val day  = 8
  val year = 2020

  def parse(src: String): Input =
    src.split('\n')
       .zipWithIndex
       .map { case s"$op $v" -> l => Instruction(strToOp(op), v.toInt, l) }
       .toList

  def strToOp(s: String): Op = s match
    case "acc" => Acc
    case "jmp" => Jmp
    case "nop" => Nop

  def partOne(in: Input): Int = eval(in, Set.empty, 0, 0)._2

  def partTwo(in: Input): Int =
    in.filter { case Instruction(op, _, _) => op != Acc }
      .toSet
      .map(i => eval(in.updated(i.label, i.flip), Set.empty, 0, 0))
      .find(_._1)
      .map(_._2)
      .get

  @tailrec
  def eval(is: Input, history: Set[Int], isp: Int, acc: Int): (Boolean, Int) =
    if (history contains isp) false -> acc
    else if (isp >= is.size) true -> acc
    else is(isp) match
      case Instruction(Jmp, offset, l) =>
        eval(is, history + l, isp + offset, acc)
      case Instruction(Acc, amount, l) =>
        eval(is, history + l, isp + 1, acc + amount)
      case Instruction(Nop, _, l) =>
        eval(is, history + l, isp + 1, acc)
