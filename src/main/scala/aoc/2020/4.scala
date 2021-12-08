package aoc.twenty20


import aoc._
import aoc.Utils._
import scala.util.matching.Regex._


object Day4 extends Exercise:

  val day       = 4
  val year      = 2020
  val fields    = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val eyeColors = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

  def process(entries: String, validate: MatchIterator => Boolean): Int =
    entries.split("(?m)^\\s*$")
           .map("""[\s]*(\w+):([#\w]+)""".r.findAllIn)
           .filter(validate)
           .size

  def validate1(keys: MatchIterator): Boolean =
    fields forall (keys.matchData map (_ group 1)).toSet.contains

  def validate2(keys: MatchIterator): Boolean =

    val md = keys.matchData.toList

    val pairs: Map[String, String] =
      md.map(_ group 1)
        .zip(md map (_ group 2))
        .toMap

    val byrOk = pairs.get("byr").map(_.toInt).getOrElse(0).between(1920, 2002)
    val eyrOk = pairs.get("eyr").map(_.toInt).getOrElse(0).between(2020, 2030)
    val iyrOk = pairs.get("iyr").map(_.toInt).getOrElse(0).between(2010, 2020)
    val eclOk = eyeColors contains pairs.getOrElse("ecl", "")
    val pidOk = "\\d{9}".r matches pairs.getOrElse("pid", "")

    val hgtOk =
      pairs.getOrElse("hgt", "") match
        case s"${cm}cm" => cm.toInt.between(150, 193)
        case s"${in}in" => in.toInt.between(59, 76)
        case _          => false

    val hclOk =
      pairs.getOrElse("hcl", "") match
        case s"#${color}" => "[a-f\\d]{6}".r matches color
        case _            => false

    byrOk && eclOk && eyrOk && hgtOk && hclOk && iyrOk && pidOk

  def partOne(in: String) = process(in, validate1)

  def partTwo(in: String) = process(in, validate2)
