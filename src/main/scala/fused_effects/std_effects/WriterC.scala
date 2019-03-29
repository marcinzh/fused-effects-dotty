package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


case class WriterC[W, M[_], A](run: StateC[W, M, A])

object WriterC {
  type Ap1[S] = [M[_], A] => WriterC[S, M, A]
  type Ap2[S, M[_]] = [A] => WriterC[S, M, A]
}


implied WriterC_Carrier[H0[_[_], _] : Effect, M0[_], W: Monoid] given (otherCarrier: Carrier[H0, M0]) for Carrier[
  ([M[_], A] => Writer[W, M, A]) :+: H0,
  [A] => WriterC[W, M0, A]
] {
  private type H = ThisH
  private type M = ThisM
  // import otherCarrier.{theHFunctor => otherHFunctor}
  import otherCarrier.{theMonad => otherMonad}
  implicit def theHFunctor: HFunctor[H] = the[HFunctor[H]]
  implicit def theMonad: Monad[M] = the[Monad[M]]

  private type WrappedCarrier = Carrier[State.Ap1[W] :+: H0, StateC.Ap2[W, M0]]
  private implicit def theWrappedCarrier : WrappedCarrier = the[WrappedCarrier]

  def eff[A](h: H[M, M[A]]): M[A] = h match {
    case Sum.L(Tell(w, wtf)) => WriterC {
      for {
        _ <- modify((_: W) |@| w)
        x <- wtf.run
      } yield x
    }
    case Sum.L(Listen(scope, wtf)) => WriterC {
      for {
        w0 <- get
        _ <- put(Monoid[W].empty)
        a <- scope.run
        w1 <- get
        // _ <- modify(w0 |@| (_: W))
        _ <- put(w0 |@| w1)
        x <- wtf(w1)(a).run
      } yield x
    }
    case Sum.L(Censor(mod, scope, wtf)) => WriterC {
      for {
        w0 <- get
        _ <- put(Monoid[W].empty)
        a <- scope.run
        _ <- modify((w: W) => w0 |@| mod(w))
        x <- wtf(a).run
      } yield x
    }
    case Sum.R(other) => WriterC {
      val ff = new ~>[WriterC.Ap2[W, M0], StateC.Ap2[W, M0]] {
        def apply[A](carr: WriterC[W, M0, A]) = carr.run
      }
      val h = other.handlePure(ff)
      the[WrappedCarrier].eff(Sum.R(h))
    }
  }
}
