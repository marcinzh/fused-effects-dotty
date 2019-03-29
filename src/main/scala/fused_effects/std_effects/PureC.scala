package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


case class PureC[A](run: A)


implied PureC_Monad for Monad[PureC] {
  private type F = ThisMonad

  def pure[A](a: A): F[A] = PureC(a)

  def (fa: F[A]) flatMap[A, B](f: A => F[B]): F[B] = f(fa.run)
}

implied Pure_Carrier for Carrier[Pure, PureC] {
  private type H = ThisH
  private type M = ThisM
  implicit def theHFunctor: HFunctor[ThisH] = Pure_Effect
  implicit def theMonad: Monad[ThisM] = PureC_Monad

  def eff[A](h: H[M, M[A]]): M[A] = h match {
    case _ => ???
  }
}
