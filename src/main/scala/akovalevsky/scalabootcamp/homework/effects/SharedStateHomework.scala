package akovalevsky.scalabootcamp.homework.effects

import cats.Monad
import cats.effect.concurrent.{Deferred, MVar, Ref}
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Resource, Timer}
import cats.syntax.all._
import cats.effect.syntax.all._
import io.chrisdavenport.log4cats.SelfAwareStructuredLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

object IosCommon {
  val logger: SelfAwareStructuredLogger[IO] = Slf4jLogger.getLogger[IO]
}

/*
 * What about combining Refs and Deferred?
 * Implement a `memoize` function that takes some `f:F[A]` and memoizes it (stores the result of computation).
 * What will happen if the function `f` will fail with some error?
 */

object RefsExerciseTwo extends IOApp {

  def memoize[F[_], A](f: F[A])(implicit C: Concurrent[F]): F[F[A]] = {
    val deferredF = Deferred[F, Either[Throwable, A]](C)
    val hasBeenExecutedRefF = Ref[F].of(false)

    for {
      deferred <- deferredF
      hasBeenExecutedRef <- hasBeenExecutedRefF
    } yield C.suspend {
      for {
        hasBeenExecuted <- hasBeenExecutedRef.getAndSet(true)
        _ <- if (!hasBeenExecuted) f.attempt.flatMap(deferred.complete) else C.pure(())
        resultOrThrowable <- deferred.get
        result <- C.fromEither(resultOrThrowable)
      } yield result
    }
  }


  override def run(args: List[String]): IO[ExitCode] = {

    val successProgram = IO {
      println("Hey!")
      42
    }

    /*
     * Should print
     * Hey!
     * 42
     * 42
     * */

    val successResult: IO[Unit] = for {
      mem <- memoize(successProgram)
      x <- mem
      _ <- IO(println(x))
      y <- mem
      _ <- IO(println(y))
    } yield ()


    val errorProgram = IO {
      println("Gonna Boom!")
      throw new IllegalArgumentException("BOOM")
    }

    /*
     * Should print
     * Gonna Boom!
     * java.lang.IllegalArgumentException: BOOM
     */

    val failedResult: IO[Unit] = for {
      mem <- memoize(errorProgram)
      x <- mem.attempt
      _ <- IO(println(x))
      y <- mem.attempt
      _ <- IO(println(y))
    } yield ()

    successResult *>
      failedResult *>
      IO(ExitCode.Success)
  }
}

/*
 * Implement race method (who completed first - wins, other should be canceled) using MVar
 * Tip: Recall that we can use Fibers in order to schedule task in background
 */

object RaceMVarExercise extends IOApp {

  def race[A](taskA: IO[A], taskB: IO[A]): IO[A] = {

    for {
      winnerResult <- MVar.empty[IO, A]
      fiberA <- (taskA >>= winnerResult.put).start
      fiberB <- (taskB >>= winnerResult.put).start

      res <- winnerResult.take <* fiberA.cancel <* fiberB.cancel
    } yield res

  }

  override def run(args: List[String]): IO[ExitCode] = {

    import IosCommon.logger

    def task(index: Int, sleepDuration: FiniteDuration): IO[Int] = {
      for {
        _ <- logger.info(s"$index is sleeping for $sleepDuration seconds")
        _ <- IO.sleep(sleepDuration)
      } yield index
    }

    for {
      index <- race(task(0, 3.seconds), task(1, 5.seconds))
      _ <- logger.info(s"index should be 0, $index ")
    } yield ExitCode.Success
  }
}

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 */
object SharedStateHomework extends IOApp {

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]
  }

  class RefCache[F[_] : Clock : Monad, K, V](
                                              state: Ref[F, Map[K, (Long, V)]],
                                              expiresIn: FiniteDuration
                                            ) extends Cache[F, K, V] {
    def get(key: K): F[Option[V]] = for {
      cache <- state.get
    } yield cache.get(key).map { case (_, value) => value }

    def put(key: K, value: V): F[Unit] = for {
      now <- Clock[F].realTime(expiresIn.unit)
      _ <- state.update(cache => cache + (key -> (now + expiresIn.length, value)))
    } yield ()
  }

  object Cache {
    def of[F[_], K, V](
                        expiresIn: FiniteDuration,
                        checkOnExpirationsEvery: FiniteDuration
                      )(implicit T: Timer[F], C: Concurrent[F]): F[Resource[F, Cache[F, K, V]]] = {

      def cacheExpirationService(state: Ref[F, Map[K, (Long, V)]]): F[Unit] = {
        def run(): F[Unit] = for {
          _ <- T.sleep(checkOnExpirationsEvery)
          now <- Clock[F].realTime(expiresIn.unit)
          _ <- state.modify {
            cache => (cache.filter { case (_, (expiresAt, _)) => expiresAt > now }, ())
          }
          _ <- run()
        } yield ()

        run()
      }

      for {
        state <- Ref.of[F, Map[K, (Long, V)]](Map[K, (Long, V)]())
        expirationService <- cacheExpirationService(state).start
      } yield Resource.make(C.delay(new RefCache[F, K, V](state, expiresIn)))(_ => expirationService.cancel)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      cache <- Cache.of[IO, Int, String](10.seconds, 4.seconds)
      _ <- cache.use { cache =>
        for {
          _ <- cache.put(1, "Hello")
          _ <- cache.put(2, "World")
          _ <- cache.get(1).flatMap(s => IO {
            println(s"first key $s")
          })
          _ <- cache.get(2).flatMap(s => IO {
            println(s"second key $s")
          })
          _ <- IO.sleep(6.seconds)
          _ <- cache.get(1).flatMap(s => IO {
            println(s"first key $s")
          })
          _ <- cache.get(2).flatMap(s => IO {
            println(s"second key $s")
          })
          _ <- IO.sleep(12.seconds)
          _ <- cache.get(1).flatMap(s => IO {
            println(s"first key $s")
          })
          _ <- cache.get(2).flatMap(s => IO {
            println(s"second key $s")
          })
        } yield ()
      }

    } yield ExitCode.Success
  }
}

