package aoc.y21


import aoc._
import aoc.Utils._
import scala.annotation.tailrec


object Day10 extends Exercise[Seq[Either[Char, String]]]:

  val day  = 10
  val year = 2021

  val matching = Map(')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<')
  val scores   = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val scores2  = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)
  val swapped  = matching map { _.swap }

  def parse(str: String) =
    @tailrec
    def go(stack: Seq[Char], str: String): Either[Char, String] =
      if str.isEmpty then Right(stack.mkString)
      else if matching.values.toSeq contains str.head then
        go(str.head +: stack, str.tail)
      else if stack.head == matching(str.head) then
        go(stack.tail, str.tail)
      else Left(str.head)

    str split '\n' map { go(List(), _) }

  def partOne(in: Input): Int =
    in.map {
      case Left(c) => scores(c)
      case _       => 0 }
      .sum

  def partTwo(in: Input) =
    val theScores = 
      in.map {
        case Left(_)    => 0
        case Right(str) =>
          str.foldLeft(0: Long) {
            case (acc, next) => acc * 5 + scores2(swapped(next)) } }
        .sorted
        .filter { _ != 0 }

    theScores(1 + theScores.size / 2)
