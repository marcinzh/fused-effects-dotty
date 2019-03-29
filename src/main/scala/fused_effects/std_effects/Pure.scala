package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


sealed trait Pure[M[_], A]

object Pure {
  type Ap2[M[_]] = [A] => Pure[M, A]
}


implied Pure_Effect for Effect[Pure] {
  private type H = ThisEffect

  def (h: H[M, A]) fmap[M[_], A, B](f: A => B): H[M, B] = h match {
  	case _ => ???
  }

  def (h: H[M, A]) hmap[M[_], N[_], A](ff: M ~> N): H[N, A] = h match {
  	case _ => ???
  }

  def (h: H[M, M[A]]) handle[F[_] : Functor, M[_], N[_], A](
    fu: F[Unit],
    ff: ([X] => F[M[X]]) ~> ([X] => N[F[X]])
  ): H[N, N[F[A]]] = h match {
  	case _ => ???
  }
}
