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
      _   <- M pure println(s"Day $day ran $a, result : $out")
    yield
      out


type ExerciseWithInputFile = Exercise[String]


object Utils:

  inline def M[F[_] : Monad] = summon

  def readFile[F[_] : Monad : Sync](path: String): F[String] =
    Resource.fromAutoCloseable(M pure Source.fromFile(path))
            .use(M pure _.mkString)
