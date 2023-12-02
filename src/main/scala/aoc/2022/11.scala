package aoc.y22


import aoc._
import aoc.Utils._
import scala.annotation.tailrec


case class Monkey( items: List[Long], div: Int, op: Long => Long
                 , give: Long => Int, n: Long)


object Day11 extends Exercise[Array[Monkey]]:

  val day  = 11
  val year = 2022

  val re =
    """(?s)Monkey (\d+):
      |(\s+\w+)*:([\d\s,]+)
      |(\s+\w+)*: new = ([\d\s\w\+\*]+)
      |(\s+\w+)*: divisible by (\d+)
      |(\s+\w+)*: throw to monkey (\d+)
      |(\s+\w+)*: throw to monkey (\d+)""".stripMargin.r


  def parse(str: String) =
    str.strip
       .split("\n\n")
       .map { case re(_, _, items, _, op, _, div, _, t, _, f) =>
         Monkey( items.split(',').map(_.strip.toLong).toList
               , div.toInt, parseOp(op)
               , x => if x % div.toInt == 0 then t.toInt else f.toInt, 0) }


  def parseOp(op: String) =
    val f =
      if op contains '+' then (i: Long) => (j: Long) => i + j
      else (i: Long) => (j: Long) => i * j

    """\d+""".r.findFirstIn(op) match
      case Some(x) => f(x.toInt)
      case None    => (x: Long) => f(x)(x)


  @tailrec
  def runRounds(in: Input, i: Int, rounds: Int, scale: Long => Long):
    Input =
    if rounds == 0 then in
    else if i == in.size then runRounds(in, 0, rounds - 1, scale)
    else
      val mk = in(i)
      if mk.items.isEmpty then runRounds(in, i + 1, rounds, scale)
      else
        val wl  = scale(mk.op(mk.items.head))
        val mk2 = mk give wl

        runRounds(in.updated(i, mk.copy(items = mk.items.tail, n = mk.n + 1))
                    .updated(mk2, in(mk2).copy(items = in(mk2).items :+ wl))
                 , i, rounds, scale)


  def monkeyBusiness(in: Input, rounds: Int, scale: Long => Long) =
    runRounds(in, 0, rounds, scale)
      .map(_.n)
      .sorted(using Ordering[Long].reverse)
      .take(2)
      .product


  def partOne(in: Input) = monkeyBusiness(in, 20, _ / 3)

  def partTwo(in: Input) = monkeyBusiness(in, 10000, _ % in.map(_.div).product)
