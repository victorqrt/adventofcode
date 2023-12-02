package aoc.y22


import aoc._
import aoc.Utils._
import scala.annotation.tailrec


// TODO: originalIdx -> (value, idx)
// object Day20_2 extends Exercise[Map[Int, (Long, Int)]]:
object Day20 extends Exercise[Array[(Long, Int)]]:

  val day  = 20
  val year = 2022

  def parse(str: String) =
    str.split('\n')
       .map(_.toLong)
       .zipWithIndex

  def partOne(in: Input) = decrypt(in, 1)

  def partTwo(in: Input) =
    decrypt(in.map { case l -> i => (l * 811589153) -> i }, 10)

  def move(arr: Input, idx: Int) =
    val ((n, _), i)  = arr.zipWithIndex.find(_._1._2 == idx).get
    val jmp          = (n + i) % (arr.size - 1)
    val newIdx       = if jmp > 0 then jmp else arr.size - 1 + jmp
    val (head, tail) = arr.filter(_._2 != idx) splitAt newIdx.toInt

    (head :+ (n -> idx)) ++ tail

  @tailrec
  def mix(arr: Input, n: Int): Input =
    @tailrec
    def go(_arr: Input, i: Int): Input =
      if i == _arr.size then _arr
      else go(move(_arr, i), i + 1)

    if n == 0 then arr
    else mix(go(arr, 0), n - 1)

  def decrypt(in: Input, n: Int) =
    val arr  = mix(in, n).map(_._1)
    val zero = arr indexOf 0
    val i1   = (zero + 1000) % arr.size
    val i2   = (zero + 2000) % arr.size
    val i3   = (zero + 3000) % arr.size

    arr(i1) + arr(i2) + arr(i3)
