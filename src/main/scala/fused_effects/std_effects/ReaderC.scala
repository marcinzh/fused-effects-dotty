package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


case class ReaderC[E, M[_], A](run: E => M[A])


implied ReaderC_Monad[M[_] : Monad, E] for Monad[[A] => ReaderC[E, M, A]] {
  private type F = ThisMonad

  def pure[A](a: A): F[A] = ReaderC(_ => Monad[M].pure(a))

  def (fa: F[A]) map[A, B](f: A => B): F[B] = ReaderC(e => fa.run(e).map(f))

  def (fa: F[A]) flatMap[A, B](f: A => F[B]): F[B] = ReaderC(e => fa.run(e).flatMap(f.andThen(_.run(e))))
}
