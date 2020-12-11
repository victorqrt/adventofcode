package aoc_2020


import cats.effect._
import cats.implicits._
import cats.Monad
import scala.io.Source
import Utils._


trait Exercise[In]:

  type Out

  def day: Int

  def run[F[_] : Monad : Sync](a: In): F[Out]

  def runWithReport[F[_] : Monad : Sync](a: In): F[Out] =
    for
      out <- run[F](a)
      _   <- S delay println(s"Day $day ran $a, result : $out")
    yield
      out


type ExerciseWithInputFile = Exercise[String]


object Utils:

  inline def M[F[_] : Monad] = summon
  inline def S[F[_] : Sync]  = summon

  extension (b: Boolean) def toInt: Int = if (b) 1 else 0

  extension [T](t: T)(using o: Ordering[T])
    def between(l: T, u: T): Boolean = o.gteq(t, l) && o.gteq(u, t)

  def readFile[F[_] : Sync](path: String): F[String] =
    Resource.fromAutoCloseable(S delay Source.fromFile(path))
            .use(S delay _.mkString)
