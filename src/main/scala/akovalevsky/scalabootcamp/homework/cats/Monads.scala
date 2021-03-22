package akovalevsky.scalabootcamp.homework.cats

import cats.Monad

object Monads {

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case Some(x) => f(x)
      case None => None
    }

    override def pure[A](x: A): Option[A] = Some(x)

    // cats-only method, don't implement me
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = ???
  }

  def eitherMonad[T]: Monad[Either[T, *]] = new Monad[Either[T, *]] {
    override def flatMap[A, B](fa: Either[T, A])(f: A => Either[T, B]): Either[T, B] = fa match {
      case Right(x) => f(x)
      case Left(x) => Left[T, B](x)
    }

    override def pure[A](x: A): Either[T, A] = Right(x)

    // cats-only method, don't implement me
    override def tailRecM[A, B](a: A)(f: A => Either[T, Either[A, B]]): Either[T, B] = ???
  }

  // intentionally replaced A => B by Function1[A, B] for better understanding
  def functionMonad[T]: Monad[Function1[T, *]] = new Monad[Function1[T, *]] {
    override def flatMap[A, B](fa: Function1[T, A])(f: A => Function1[T, B]): Function1[T, B] =
      x => f(fa(x))(x)

    override def pure[A](x: A): T => A = _ => x

    // cats-only method, don't implement me
    override def tailRecM[A, B](a: A)(f: A => T => Either[A, B]): T => B = ???
  }
}
