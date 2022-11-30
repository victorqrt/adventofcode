package aoc.y20


import aoc._
import aoc.Utils._
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq


object Day7 extends Exercise[Map[String, Seq[String]]]:

  val day  = 7
  val year = 2020

  def parse(str: String) = str.split("\n").map(parseLine).toMap

  def parseLine(line: String): (String, Seq[String]) =
    line match
      case s"$color bags contain $contents." =>
        color -> contents.replaceAll("bags?", "")
                         .split(",")
                         .map(_.trim)
                         .toIndexedSeq

  def partOne(in: Input): Int = one("shiny gold", in)
  def partTwo(in: Input): Int = two("shiny gold", in)

  def one(color: String, rules: Input): Int =

    @tailrec
    def go(todo: Set[String], acc: Set[String]): Set[String] =
      if (todo.isEmpty) acc
      else
        val next = rules.filter {
          case _ -> v => v.map(_ drop 2) contains todo.head }.keySet

        go(todo.tail ++ next, acc ++ next)

    go(Set(color), Set.empty).size

  def two(color: String, rules: Input): Int =

    @tailrec
    def go(todo: ArraySeq[String], acc: Int): Int =
      if (todo.isEmpty) acc
      else
        val next = rules(todo.head) flatMap {
          case "no other"   => ArraySeq.empty[String]
          case s"$n $color" => ArraySeq.fill(n.toInt)(color) }

        go(todo.tail ++ next, acc + next.size)

    go(ArraySeq(color), 0)
