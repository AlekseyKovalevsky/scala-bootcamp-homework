package akovalevsky.scalabootcamp.homework.catseffect

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/*
 * Homework 1. Provide your own implementation of a subset of `IO` functionality.
 *
 * Provide also tests for this functionality in EffectsHomework1Spec (which you should create).
 *
 * Refer to:
 *  - https://typelevel.org/cats-effect/datatypes/io.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO$.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO.html
 * about the meaning of each method as needed.
 *
 * There are two main ways how to implement IO:
 * - Executable encoding  - express every constructor and operator for our model in terms of its execution
 * - Declarative encoding - express every constructor and operator for our model as pure data in a recursive
 *                          tree structure
 *
 * While the real Cats Effect IO implementation uses declarative encoding, it will be easier to solve this
 * task using executable encoding, that is:
 *  - Add a `private val run: () => A` parameter to the class `IO` private constructor
 *  - Have most of the methods return a `new IO(...)`
 *
 * Ask questions in the bootcamp chat if stuck on this task.
 */

object EffectsHomework1 {

  cats.effect.IO

  final class IO[A] private(private val run: () => A) {
    def map[B](f: A => B): IO[B] = new IO(() => f(run()))

    def flatMap[B](f: A => IO[B]): IO[B] = new IO(() => f(run()).run())

    def *>[B](another: IO[B]): IO[B] = new IO(() => {
      run()
      another.run()
    })

    def as[B](newValue: => B): IO[B] = new IO(() => {
      run()
      newValue
    })

    def void: IO[Unit] = new IO(() => {
      run()
      ()
    })

    def attempt: IO[Either[Throwable, A]] = new IO(() => Try(run()).toEither)

    def option: IO[Option[A]] = new IO(() => Try(run()).toOption)

    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = new IO(() => {
      Try(run()) match {
        case Success(x) => x
        case Failure(ex) => f(ex).run()
      }
    })

    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = new IO(() => {
      Try(run()) match {
        case Success(x) => map(x)
        case Failure(ex) => recover(ex)
      }
    })

    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = new IO(() => {
      Try(run()) match {
        case Success(x) => bind(x).run()
        case Failure(ex) => recover(ex).run()
      }
    })

    def unsafeRunSync(): A = run()

    def unsafeToFuture(implicit executionContext: ExecutionContext): Future[A] = Future(run())
  }

  object IO {
    def apply[A](body: => A): IO[A] = new IO(() => body)

    def suspend[A](thunk: => IO[A]): IO[A] = new IO(() => thunk.run())

    def delay[A](body: => A): IO[A] = new IO(() => body)

    def pure[A](a: A): IO[A] = new IO(() => a)

    def fromEither[A](e: Either[Throwable, A]): IO[A] = new IO(() =>
      e match {
        case Left(ex) => throw ex
        case Right(x) => x
      })

    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = new IO(() =>
      option match {
        case Some(x) => x
        case None => throw orElse
      })

    def fromTry[A](t: Try[A]): IO[A] = new IO(() =>
      t match {
        case Success(x) => x
        case Failure(ex) => throw ex
      })

    def none[A]: IO[Option[A]] = new IO(() => None)

    def raiseError[A](e: Throwable): IO[A] = new IO(() => throw e)

    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = new IO(() =>
      if (cond) ()
      else throw e
    )

    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = new IO(() =>
      if (cond) throw e
      else ()
    )

    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = new IO(() =>
      if (cond) ()
      else action.run()
    )

    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = new IO(() =>
      if (cond) action.run()
      else ()
    )

    val unit: IO[Unit] = new IO(() => ())
  }
}
