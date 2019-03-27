package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


case class ErrorC[E, M[_], A](run: M[Either[E, A]])


implied ErrorC_Monad[M[_] : Monad, E] for Monad[[A] => ErrorC[E, M, A]] {
  private type F = ThisMonad
  
  def pure[A](a: A): F[A] = ErrorC(Monad[M].pure(Right(a)))

  def (fa: F[A]) map[A, B](f: A => B): F[B] =
    ErrorC(fa.run.map {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    })

  def (fa: F[A]) flatMap[A, B](f: A => F[B]): F[B] =
    ErrorC(fa.run.flatMap {
      case Right(a) => f(a).run
      case Left(e) => Monad[M].pure(Left(e))
    })
}
