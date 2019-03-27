package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


sealed trait Error[E, M[_], A]
case class Throw[E, M[_], A](wtf: E) extends Error[E, M, A]
case class Catch[E, M[_], A, B](scope: M[B], catcher: E => M[B], wtf: B => A) extends Error[E, M, A]


implied Error_Effect[E] for Effect[[M[_], A] => Error[E, M, A]] {
  private type H = ThisHFunctor

  def (h: H[M, A]) fmap[M[_], A, B](f: A => B): H[M, B] = h match {
    case Throw(wtf) => Throw(wtf)
    case Catch(scope, catcher, wtf) => Catch(scope, catcher, wtf.andThen(f))
  }

  def (h: H[M, A]) hmap[M[_], N[_], A](ff: M ~> N): H[N, A] = h match {
    case Throw(wtf) => Throw(wtf)
    case Catch(scope, catcher, wtf) => Catch(ff(scope), catcher.andThen(ff(_)), wtf)
  }

  def (h: H[M, M[A]]) handle[F[_] : Functor, M[_], N[_], A](
    fu: F[Unit],
    ff: ([X] => F[M[X]]) ~> ([X] => N[F[X]])
  ): H[N, N[F[A]]] = h match {
    case Throw(wtf) => Throw(wtf)
    case Catch(scope: M[tB], catcher, wtf) =>
      val scope2 = ff(fu.mapConst(scope))
      val catcher2 = catcher andThen (fu.mapConst(_)) andThen (ff(_))
      val wtf2 = (fb: F[tB]) => ff(fb.map(wtf))
      Catch(scope2, catcher2, wtf2)
  }
}
