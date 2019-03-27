package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


sealed trait Reader[E, M[_], A]
case class Ask[E, M[_], A](wtf: E => A) extends Reader[E, M, A]
case class Local[E, M[_], A, B](mod: E => E, scope: M[B], wtf: B => A) extends Reader[E, M, A]


implied Reader_Effect[E] for Effect[[M[_], A] => Reader[E, M, A]] {
  private type H = ThisEffect

  def (h: H[M, A]) fmap[M[_], A, B](f: A => B): H[M, B] = h match {
    case Ask(wtf) => Ask(wtf.andThen(f))
    case Local(mod, scope, wtf) => Local(mod, scope, wtf.andThen(f))
  }

  def (h: H[M, A]) hmap[M[_], N[_], A](ff: M ~> N): H[N, A] = h match {
    case Ask(wtf) => Ask(wtf)
    case Local(mod, scope, wtf) => Local(mod, ff(scope), wtf)
  }

  def (h: H[M, M[A]]) handle[F[_] : Functor, M[_], N[_], A](
    fu: F[Unit],
    ff: ([X] => F[M[X]]) ~> ([X] => N[F[X]])
  ): H[N, N[F[A]]] = h match {
    case Ask(wtf) => Ask(wtf andThen (fu.mapConst(_)) andThen (ff(_)))
    case Local(mod, scope: M[tB], wtf) =>
      val scope2 = ff(fu.mapConst(scope))
      val wtf2 = (fb: F[tB]) => ff(fb.map(wtf))
      Local(mod, scope2, wtf2)
  }
}
