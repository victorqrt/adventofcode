package aoc_2020


import cats.effect.Sync
import cats.implicits._
import cats.Monad
import Utils._
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq


object Day7 extends ExerciseWithInputFile:

  type Out = String
  val day  = 7

  def run[F[_] : Monad : Sync](path: String): F[Out] =
    for
      in <- readFile[F](path)
      r  <- M pure getRules(in)
      p1 <- M pure partOne("shiny gold", r)
      p2 <- M pure partTwo("shiny gold", r)
    yield
      s"part 1 -> $p1, part 2 -> $p2"

  def parse(line: String): (String, Seq[String]) =
    line match
      case s"$color bags contain $contents." =>
        color -> contents.replaceAll("bags?\\.?", "")
                         .split(",")
                         .map(_.trim)
                         .toIndexedSeq

  def getRules(in: String): Map[String, Seq[String]] =
    in.split("\n")
      .map(parse)
      .toMap

  def partOne(color: String, rules: Map[String, Seq[String]]): Int =

    @tailrec
    def go(todo: Set[String], acc: Set[String]): Set[String] =
      if (todo.isEmpty) acc
      else
        val next = rules.filter {
          case _ -> v => v.map(_ drop 2) contains todo.head }.keySet

        go(todo.tail ++ next, acc ++ next)

    go(Set(color), Set.empty).size

  def partTwo(color: String, rules: Map[String, Seq[String]]): Int =

    @tailrec
    def go(todo: ArraySeq[String], acc: Int): Int =
      if (todo.isEmpty) acc
      else
        val next = rules(todo.head) flatMap {
          case "no other"   => ArraySeq.empty[String]
          case s"$n $color" => ArraySeq.fill(n.toInt)(color) }

        go(todo.tail ++ next, acc + next.size)

    go(ArraySeq(color), 0)
