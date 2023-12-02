package aoc.y23


import aoc._
import aoc.Utils._


type StepsMap = Map[String, List[(Long, Long, Long)]]

case class Almanac(seeds: List[Long], steps: StepsMap):

  lazy val stepOrder = List
    ( "seed-to-soil", "soil-to-fertilizer"
    , "fertilizer-to-water", "water-to-light"
    , "light-to-temperature", "temperature-to-humidity"
    , "humidity-to-location"
    )

  lazy val seedsAsRanges =
    seeds
      .view
      .grouped(2)
      .flatMap(l => l.head.until(l.head + l.last))

  lazy val ranges =
    steps.mapValues: triplets =>
      triplets.map:
        case (dst, src, len) =>
          (src until (src + len)) -> (dst until (dst + len))

  def advanceStep(value: Long, step: String) =
    val rngs = ranges(step).find(_._1.contains(value))
    rngs match
      case Some(src -> dst) => value - src.head + dst.head
      case _                => value

  def location(seed: Long) =
    stepOrder.foldLeft(seed)(advanceStep)


object Day5 extends Exercise[Almanac]:

  val day  = 5
  val year = 2023

  def parse(str: String) =

    val lines = str.split("\n")
    val seeds = lines.head.strip match
      case s"seeds: $seeds" => seeds.split(" ").map(_.toLong).toList

    def go(lines: List[String], acc: StepsMap = Map()): StepsMap =
      if lines.isEmpty then return acc
      val (curr, next) = lines
        .tail
        .span(!_.isBlank)

      if curr.size < 2 then acc
      else
        val step   = curr.head.split(" ").head
        val ranges = curr
          .tail
          .map:
            case s"$dst $src $len" =>
              (dst.strip.toLong, src.strip.toLong, len.strip.toLong)

        go(next, acc + (step -> ranges))

    Almanac(seeds, go(lines.tail.toList))


  def partOne(in: Input) =
    in.seeds.map(in.location).min

  def partTwo(in: Input) =
    s"56931769 [TODO]"
    // TODO slow af
    // in.seedsAsRanges.map(in.location).min
