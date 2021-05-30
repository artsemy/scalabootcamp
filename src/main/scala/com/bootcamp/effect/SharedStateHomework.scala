package com.bootcamp.effect

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Timer}
import cats.implicits._

import scala.annotation.tailrec
import scala.concurrent.duration._

object SharedStateHomework extends IOApp {

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]
  }

  class RefCache[F[_] : Clock : Monad, K, V](state: Ref[F, Map[K, (Long, V)]],
                                             expiresIn: FiniteDuration) extends Cache[F, K, V] {

    def get(key: K): F[Option[V]] = for {
//      time <- Clock[F].realTime(MILLISECONDS)
//      _ <- println("get " + time%10000).pure[F]
      map <- state.get
      value <- map.get(key).map( {case (t, v) => v} ).pure[F]
    } yield(value)

    def put(key: K, value: V): F[Unit] = for {
      time <- Clock[F].realTime(MILLISECONDS)
//      _ <- println("put " + (time + expiresIn.toMillis)%10000).pure[F]
      _ <- state.update(map => map.updated(key, (time + expiresIn.toMillis, value)))
    } yield()

    def clean(): F[Unit] = for {
      time <- Clock[F].realTime(MILLISECONDS)
      _ <- state.update(map => map.filter({case (k,(t,v)) => t > time}))
    } yield()

    //    def get(key: K): F[Option[V]] = {
    //      val time = Clock[F].realTime(MILLISECONDS).toString.substring(3).toLong
    //      println("getTime " + time%10000)
    //      state.get.map(myMap => myMap.get(key).map({case (t, v) => v})) //{}
    //    }
    //
    //    def put(key: K, value: V): F[Unit] = {
    //      val time = Clock[F].realTime(MILLISECONDS).toString.substring(3).toLong
    //      println("putTime " + (time+expiresIn.toMillis)%10000)
    //      state.update(map => map.updated(key, (time + expiresIn.toMillis, value)))
    //    }
    //
    //    def clean(): F[Unit] = {
    //      val time = Clock[F].realTime(MILLISECONDS).toString.substring(3).toLong
    //      state.update(map => map.filter({case (k,(t,v)) => t > time}))
    //    }

  }

  object Cache {
    def of[F[_] : Clock, K, V](
                                expiresIn: FiniteDuration,
                                checkOnExpirationsEvery: FiniteDuration
                              )(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {

      def loop(cache: RefCache[F, K, V]): F[Unit] = for {
        _ <- T.sleep(checkOnExpirationsEvery)
        _ <- cache.clean()
        _ <- loop(cache)
      } yield ()

      for {
        cache <- Ref.of[F, Map[K, (Long, V)]](Map()).map {state => new RefCache(state, expiresIn)}
        _ <- C.start(loop(cache))
      } yield (cache)
    }

  }

  val expiresInTime = 0.5.seconds
  val checkOnExpirationsEveryTime = 0.2.seconds
  val sleep1Time = 0.4.seconds
  val sleep2Time = 0.2.seconds

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      cache <- Cache.of[IO, Int, String](expiresInTime, checkOnExpirationsEveryTime)
      _ <- cache.put(1, "Hello")
      _ <- cache.put(2, "World")
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(sleep1Time)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(sleep2Time)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
    } yield ExitCode.Success
  }

}
