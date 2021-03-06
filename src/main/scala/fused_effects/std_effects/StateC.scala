package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


case class StateC[S, M[_], A](run: S => M[(S, A)])

object StateC {
  type Ap1[S] = [M[_], A] => StateC[S, M, A]
  type Ap2[S, M[_]] = [A] => StateC[S, M, A]
}


implied StateC_Monad[M[_] : Monad, S] for Monad[StateC.Ap2[S, M]] {
  private type F = ThisMonad

  def pure[A](a: A): F[A] = StateC(s => Monad[M].pure((s, a)))

  def (fa: F[A]) flatMap[A, B](f: A => F[B]): F[B] =
    StateC { s0 =>
      fa.run(s0).flatMap {
        case (s1, a) => f(a).run(s1)
      }
    }
}


implied StateC_Carrier[H0[_[_], _] : Effect, M0[_], S] given (otherCarrier: Carrier[H0, M0]) for Carrier[
  State.Ap1[S] :+: H0,
  StateC.Ap2[S, M0]
] {
  private type H = ThisH
  private type M = ThisM
  // import otherCarrier.{theHFunctor => otherHFunctor}
  // import otherCarrier.{theMonad => otherMonad}
  implicit def theHFunctor: HFunctor[H] = the[HFunctor[H]]
  implicit def theMonad: Monad[M] = the[Monad[M]]

  def eff[A](h: H[M, M[A]]): M[A] = h match {
    case Sum.L(Get(wtf)) => StateC(s => wtf(s).run(s))
    case Sum.L(Put(s, wtf)) => StateC(_ => wtf.run(s))
    case Sum.R(other) => StateC { s0 =>
      val ff = new ~>[[X] => (S, StateC[S, M0, X]), [X] => M0[(S, X)]] {
        def apply[A](s_carr: (S, StateC[S, M0, A])) = {
          val (s, carr) = s_carr
          carr.run(s)
        }
      }
      val h0 = the[Effect[H0]].handle(other)((s0, ()), ff)
      otherCarrier.eff(h0)
    }
  }
}
