package aoc


import cats.effect._
import cats.effect.std._
import cats.implicits._
import io.Source
import Utils._


trait Exercise[A]:

  type Input = A

  def day: Int
  def year: Int

  def parse(str: String): Input

  def partOne(in: Input): Any
  def partTwo(in: Input): Any

  def run[F[_] : Clock : Concurrent : Console]
    (input: String) =
    for
      s  <- Clock[F].monotonic
      in  = parse(input)
      p1 <- Concurrent[F].start(partOne(in).pure[F])
      p2 <- Concurrent[F].start(partTwo(in).pure[F])
      r1 <- p1.join
      r2 <- p2.join
      s1 <- r1.embedError
      s2 <- r2.embedError
      f  <- Clock[F].monotonic
      dt  = (f - s).toMillis
    yield
      s"[$year] [Day $day] [$dt ms] $s1, $s2"


object Utils:

  lazy val cpus = Runtime.getRuntime.availableProcessors

  def readFile[F[_] : Sync](path: String): F[String] =
    Resource
      .fromAutoCloseable(Source.fromFile(path).pure[F])
      .use(s => Sync[F].blocking(s.mkString))

  extension (b: Boolean)
    def toInt: Int = if b then 1 else 0

  extension (s: String)
    def diff(s2: String): Set[Char] =
      s.toArray.toSet -- s2.toArray.toSet

    def inside(s2: String): Boolean =
      s.toArray.toSet subsetOf s2.toArray.toSet

    def negateBits: String = s.map:
      case '0' => '1'
      case _   => '0'

  extension [K, V](m: Map[K, V])
    def keyOf(v: V): K = m.find((_, _v) => _v == v).get._1

  extension [T](t: T)(using o: Ordering[T])
    def between(l: T, u: T): Boolean = o.gteq(t, l) && o.gteq(u, t)

  extension (is: Iterable[Long])
    def lcm = is
      .foldLeft(1L):
        case a -> b =>
          a * b /
          Stream
            .iterate(a -> b):
              case x -> y => y -> x % y
            .dropWhile(_._2 != 0L)
            .head
            ._1
            .abs
