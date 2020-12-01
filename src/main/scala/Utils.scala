package aoc_2020


import cats.effect._
import cats.implicits._
import cats.Monad
import scala.io.Source


trait Exercise[A, B]:

  def day: Int

  def run[F[_] : Effect : LiftIO : Monad](a: A): F[B]

  def runWithReport[F[_] : Effect : LiftIO : Monad](a: A): F[B] =
    for
      res <- run[F](a)
      _   <- IO(println(s"Day $day ran on $a, result : $res")).to[F]
    yield
      res


object Utils:

  def readFile[F[_] : Effect : LiftIO : Monad](path: String): F[String] =
    Resource.fromAutoCloseable(IO(Source.fromFile(path)).to[F])
            .use(source => IO(source.mkString).to[F])
