package sandbox.chapter4_monads.exercises

object FunctorImpl_412 {
  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
    def map[A, B](value: F[A])(func: A => B): F[B] = {
      val f = pure(func)
      flatMap(value)(a => pure(func(a)))
    }
  }
}
