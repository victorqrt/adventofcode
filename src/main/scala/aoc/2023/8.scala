package aoc.y23


import aoc._
import aoc.Utils.lcm


final case class PathAndMap(path: String, maps: Map[String, (String, String)]):

  def travel(start: (Long, String), arrived: String => Boolean):
    (Long, String) =

    val end = path
      .foldLeft(start):
        case n -> current -> _ if arrived(current) => (n, current)
        case n -> current -> 'L' => (n + 1, maps(current)._1)
        case n -> current -> 'R' => (n + 1, maps(current)._2)

    if arrived(end._2) then end
    else travel(end, arrived)


object Day8 extends Exercise[PathAndMap]:

  val day  = 8
  val year = 2023

  def parse(str: String) =
    val split = str.split("\n")
    val path  = split.head
    val maps  = split
      .drop(2)
      .map:
        case s"$current = ($left, $right)" =>
          current -> (left, right)
      .toMap

    PathAndMap(path, maps)


  def partOne(in: Input) = in.travel(0L -> "AAA", _ == "ZZZ")._1

  def partTwo(in: Input) =
    in.maps
      .keys
      .filter(_ endsWith "A")
      .map(start => in.travel(0L -> start, _ endsWith "Z")._1)
      .lcm
