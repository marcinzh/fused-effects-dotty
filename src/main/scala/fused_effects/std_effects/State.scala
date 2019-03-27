package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


sealed trait State[S, M[_], A]
case class Get[S, M[_], A](wtf: S => A) extends State[S, M, A]
case class Put[S, M[_], A](s: S, wtf: A) extends State[S, M, A]


implied State_Effect[S] for Effect[[M[_], A] => State[S, M, A]] {
  private type H = ThisHFunctor

  def (h: H[M, A]) fmap[M[_], A, B](f: A => B): H[M, B] = h match {
    case Get(wtf) => Get(wtf.andThen(f))
    case Put(s, wtf) => Put(s, f(wtf))
  }

  def (h: H[M, A]) hmap[M[_], N[_], A](ff: M ~> N): H[N, A] = h match {
    case Get(wtf) => Get(wtf)
    case Put(s, wtf) => Put(s, wtf)
  }

  def (h: H[M, M[A]]) handle[F[_] : Functor, M[_], N[_], A](
    fu: F[Unit],
    ff: ([X] => F[M[X]]) ~> ([X] => N[F[X]])
  ): H[N, N[F[A]]] = h match {
    case Get(wtf) => Get(wtf andThen (fu.mapConst(_)) andThen (ff(_)))
    case Put(s, wtf) => Put(s, ff(fu.mapConst(wtf)))
  }
}
