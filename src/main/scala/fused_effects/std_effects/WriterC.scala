package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


case class WriterC[W, M[_], A](run: StateC[W, M, A])


implied WriterC_Carrier[H0[_[_], _] : Effect, M0[_], W: Monoid] given (otherCarrier: Carrier[H0, M0]) for Carrier[
  ([M[_], A] => Writer[W, M, A]) :+: H0,
  [A] => WriterC[W, M0, A]
] {
  private type H = ThisH
  private type M = ThisM
  // import otherCarrier.{require_HFunctor => otherHFunctor}
  // import otherCarrier.{require_Monad => otherMonad}
  implicit def require_HFunctor: HFunctor[H] = the[HFunctor[H]]
  implicit def require_Monad: Monad[M] = the[Monad[M]]

  def eff[A](h: H[M, M[A]]): M[A] = h match {
    case Sum.L(Tell(w, wtf)) => ???
    case Sum.L(Listen(scope, wtf)) => ???
    case Sum.L(Censor(mod, scope, wtf)) => ???
    case Sum.R(other) => ???
  }
}
