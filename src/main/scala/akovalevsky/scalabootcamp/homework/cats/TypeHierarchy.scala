package akovalevsky.scalabootcamp.homework.cats

import cats.Functor


object TypeHierarchy {

  trait Applicative[F[_]] extends Functor[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def unit[A](a: => A): F[A]

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(map(fa)(a => (b: B) => f(a, b)))(fb)

    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
      map2(fab, fa)((f, a) => f(a))

    def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(x => x)

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = sequence(as.map(f))
  }

  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(a => f(a)))

    def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => unit(f(x))) // why flatMap(ma)(unit compose f) doesn't compile?


    def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))
  }

}
