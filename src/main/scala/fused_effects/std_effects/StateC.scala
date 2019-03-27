package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


case class StateC[S, M[_], A](run: S => M[(S, A)])


implied StateC_Monad[M[_] : Monad, S] for Monad[[A] => StateC[S, M, A]] {
  private type F = ThisMonad

  def pure[A](a: A): F[A] = StateC(s => Monad[M].pure((s, a)))

  def (fa: F[A]) map[A, B](f: A => B): F[B] =
    StateC { s0 =>
      fa.run(s0).map {
        case (s1, a) => (s1, f(a))
      }
    }

  def (fa: F[A]) flatMap[A, B](f: A => F[B]): F[B] =
    StateC { s0 =>
      fa.run(s0).flatMap {
        case (s1, a) => f(a).run(s1)
      }
    }
}
