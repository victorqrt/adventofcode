package aoc


import cats.effect._
import cats.implicits._
import cats.Monad
import scala.io.Source
import scala.quoted._
import Utils._


trait Exercise:

  def day: Int
  def year: Int

  def partOne(input: String): Any
  def partTwo(input: String): Any

  def run[F[_] : Monad : Sync](path: String): F[Unit] =
    for
      in <- readFile[F](path)
      p1 = partOne(in)
      p2 = partTwo(in)
      _  <- S delay println(s"[$year] [Day $day] $p1, $p2")
    yield
      ()


object Utils:

  inline def S[F[_] : Sync]  = summon

  def readFile[F[_] : Sync](path: String): F[String] =
    Resource.fromAutoCloseable(S delay Source.fromFile(path))
            .use(S delay _.mkString)

  extension (b: Boolean)
    def toInt: Int = if b then 1 else 0

  extension (s: String)
    def negateBits: String = s map {
      case '0' => '1'
      case _   => '0'
    }

  extension (s: String) def diff(s2: String): Set[Char] =
    s.toArray.toSet -- s2.toArray.toSet

  extension (s: String) def inside(s2: String): Boolean =
    s.toArray.toSet subsetOf s2.toArray.toSet

  extension [K, V](m: Map[K, V]) def keyOf(v: V): K =
    m.find((_, _v) => _v == v).get._1

  extension [T](t: T)(using o: Ordering[T])
    def between(l: T, u: T): Boolean = o.gteq(t, l) && o.gteq(u, t)
