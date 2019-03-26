package mwords


trait Functor[F[_]] {
  final type ThisFunctor = F

  def (fa: F[A]) map[A, B](f: A => B): F[B]
}

object Functor {
  def apply[F[_]] given (ev: Functor[F]) = ev
}


trait Monad[F[_]] extends Functor[F] {
  final type ThisMonad = F

  def pure[A](a: A): F[A]
  def (fa: F[A]) flatMap[A, B](f: A => F[B]): F[B]
}

object Monad {
  def apply[F[_]] given (ev: Monad[F]) = ev
}