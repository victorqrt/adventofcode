package aoc.twenty20


import aoc._
import aoc.Utils._
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq


object Day7 extends Exercise:

  val day  = 7
  val year = 2020

  def parse(line: String): (String, Seq[String]) =
    line match
      case s"$color bags contain $contents." =>
        color -> contents.replaceAll("bags?", "")
                         .split(",")
                         .map(_.trim)
                         .toIndexedSeq

  def getRules(in: String): Map[String, Seq[String]] =
    in.split("\n")
      .map(parse)
      .toMap

  def one(color: String, rules: Map[String, Seq[String]]): Int =

    @tailrec
    def go(todo: Set[String], acc: Set[String]): Set[String] =
      if (todo.isEmpty) acc
      else
        val next = rules.filter {
          case _ -> v => v.map(_ drop 2) contains todo.head }.keySet

        go(todo.tail ++ next, acc ++ next)

    go(Set(color), Set.empty).size

  def two(color: String, rules: Map[String, Seq[String]]): Int =

    @tailrec
    def go(todo: ArraySeq[String], acc: Int): Int =
      if (todo.isEmpty) acc
      else
        val next = rules(todo.head) flatMap {
          case "no other"   => ArraySeq.empty[String]
          case s"$n $color" => ArraySeq.fill(n.toInt)(color) }

        go(todo.tail ++ next, acc + next.size)

    go(ArraySeq(color), 0)

  def partOne(in: String): Int = one("shiny gold", getRules(in))

  def partTwo(in: String): Int = two("shiny gold", getRules(in))
