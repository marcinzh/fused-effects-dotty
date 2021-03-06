package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


case class ReaderC[E, M[_], A](run: E => M[A])

object ReaderC {
  type Ap1[E] = [M[_], A] => ReaderC[E, M, A]
  type Ap2[E, M[_]] = [A] => ReaderC[E, M, A]
}


implied ReaderC_Monad[M[_] : Monad, E] for Monad[[A] => ReaderC[E, M, A]] {
  private type F = ThisMonad

  def pure[A](a: A): F[A] = ReaderC(_ => Monad[M].pure(a))

  def (fa: F[A]) flatMap[A, B](f: A => F[B]): F[B] = ReaderC(e => fa.run(e).flatMap(f.andThen(_.run(e))))
}


implied Reader_Carrier[H0[_[_], _], M0[_], E] given (otherCarrier: Carrier[H0, M0]) for Carrier[
  Reader.Ap1[E] :+: H0,
  ReaderC.Ap2[E, M0]
] {
  private type H = ThisH
  private type M = ThisM
  import otherCarrier.{theHFunctor => otherHFunctor}
  // import otherCarrier.{requireMonad => otherMonad}
  implicit def theHFunctor: HFunctor[ThisH] = the[HFunctor[ThisH]]
  implicit def theMonad: Monad[ThisM] = the[Monad[ThisM]]

  def eff[A](h: H[M, M[A]]): M[A] = h match {
    case Sum.L(Ask(wtf)) => ReaderC(e => wtf(e).run(e))
    case Sum.L(Local(mod, scope: ReaderC[E, M0, tB], wtf)) => ReaderC[E, M0, tB](e => scope.run(mod(e))).flatMap(wtf)
    case Sum.R(other) => ReaderC { e =>
      val ff = new ~>[[X] => ReaderC[E, M0, X], M0] {
        def apply[A](carr: ReaderC[E, M0, A]) = carr.run(e)
      }
      val h0 = other.handlePure(ff)
      otherCarrier.eff(h0)
    }
  }
}
