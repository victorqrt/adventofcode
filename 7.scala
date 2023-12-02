import io.Source
import math.Ordered.orderingToOrdered


extension [K, V] (m: Map[K, V])
  def keyOf(v: V) =
    m.find((_, _v) => _v == v).get._1


enum Card:
  case `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, T, J, Q, K, A


given Ordering[Card] with
  def compare(c1: Card, c2: Card) = c1.ordinal compare c2.ordinal


type Kickers2 = (Card, Card)
type Kickers3 = (Card, Card, Card)
type Kickers4 = (Card, Card, Card, Card)


enum Hand:
  case High(c: Card, kickers: Kickers4)
  case Pair(c: Card, kickers: Kickers3)
  case TwoPairs(c1: Card, c2: Card, kicker: Card)
  case Three(c: Card, kickers: Kickers2)
  case Full(c1: Card, c2: Card)
  case Four(c: Card, kicker: Card)
  case Five(c: Card)


import Hand._

given Ordering[Hand] with

  // TODO try to implement a compareMany
  def compare(h1: Hand, h2: Hand) =
    val byType = h1.ordinal compare h2.ordinal

    if byType != 0 then byType

    else (h1, h2) match
      case High(c1, ks1) -> High(c2, ks2) =>
        val byHigh = c1 compare c2
        if byHigh != 0 then byHigh
        else ks1 compare ks2

      case Pair(c1, ks1) -> Pair(c2, ks2) =>
        val byPair = c1 compare c2
        if byPair != 0 then byPair
        else ks1 compare ks2

      case TwoPairs(c11, c12, ks1) -> TwoPairs(c21, c22, ks2) =>
        val byPair1 = c11 compare c21
        if byPair1 != 0 then byPair1
        else
          val byPair2 = c12 compare c22
          if byPair2 != 0 then byPair2
          else ks1 compare ks2

      case Three(c1, ks1) -> Three(c2, ks2) =>
        val byThree = c1 compare c2
        if byThree != 0 then byThree
        else ks1 compare ks2

      case Four(c1, ks1) -> Four(c2, ks2) =>
        val byFour = c1 compare c2
        if byFour != 0 then byFour
        else ks1 compare ks2

      case Full(c11, c12) -> Full(c21, c22) =>
        val cmp = c11 compare c21
        if cmp != 0 then cmp
        else c12 compare c22

      case Five(c1) -> Five(c2) => c1 compare c2

      case _ => 0


import Card._

object Day7 extends App:

  val str = Source.fromFile("7ex.txt").mkString
  //val str = Source.fromFile("inputs/2023/7.txt").mkString

  def parse(str: String) =
    str
      .split("\n")
      .map:
        case s"$_cards $score" =>
          val h = _cards
            .map: c =>
              Card.values.find(_.toString.head == c).get
            .groupBy(identity)
            .mapValues(_.size)
            .toMap

          val counts = h.values.toList
          val cards  = h.keySet.toList.sorted.reverse

          val hand =
            if counts.contains(5) then
              Five(h.keyOf(5))

            else if counts.contains(4) then
              Four(h.keyOf(4), h.keyOf(1))

            else if counts.contains(3) && counts.contains(2) then
              Full(h.keyOf(3), h.keyOf(2))

            else if counts.contains(3) then
              val c      = h.keyOf(3)
              val others = cards.filter(_ != c)
              Three(c, (others(0), others(1)))

            else if counts.count(_ == 2) == 2 then
              val pairs = h.filter((_, v) => v == 2).toList
              val p1    = pairs(0)._1
              val p2    = pairs(1)._1
              val hi    = if p1 > p2 then p1 else p2
              val lo    = if p1 > p2 then p2 else p1
              val other = cards.find(c => c != p1 && c != p2).get
              TwoPairs(hi, lo, other)

            else if counts.contains(2) then
              val pair   = h.keyOf(2)
              val others = cards.filter(_ != pair)
              Pair(pair, (others(0), others(1), others(2)))

            else
              High(cards(0), (cards(1), cards(2), cards(3), cards(4)))

          score.toLong -> hand
      .sortBy(_._2)
      .reverse


  val in = parse(str)

  in.foreach:
    case s -> h => println(s"[hand: $h, score: $s]")

  val partOne = in
    .view
    .map(_._1)
    .zipWithIndex
    .map:
      case s -> i => s * (1 + i)
    .sum

  println(partOne)
