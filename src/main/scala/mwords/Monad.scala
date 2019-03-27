package mwords


trait Functor[F[_]] {
  final type ThisFunctor = F

  def (fa: F[A]) map[A, B](f: A => B): F[B]

  def (fa: F[A]) mapConst[A, B](b: B): F[B] = fa.map(_ => b)
}

object Functor {
  def apply[F[_]] given (ev: Functor[F]) = ev

  implied Pair_Functor[S] for Functor[[X] => (S, X)] {
    private type F = ThisFunctor
    def (fa: F[A]) map[A, B](f: A => B): F[B] = (fa._1, f(fa._2))
  }

  implied Either_Functor[E] for Functor[[X] => Either[E, X]] {
    private type F = ThisFunctor
    def (fa: F[A]) map[A, B](f: A => B): F[B] = fa match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }
  }
}


trait Monad[F[_]] extends Functor[F] {
  final type ThisMonad = F

  def pure[A](a: A): F[A]
  def (fa: F[A]) flatMap[A, B](f: A => F[B]): F[B]

  def (fa: F[A]) default_map[A, B](f: A => B): F[B] = fa.flatMap(a => pure(f(a)))
}

object Monad {
  def apply[F[_]] given (ev: Monad[F]) = ev
}
