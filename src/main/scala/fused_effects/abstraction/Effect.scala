package fused_effects.abstraction
import mwords._


trait Effect[H[_[_], _]] extends HFunctor[H] {
  type ThisEffect = H

  def (h: H[M, M[A]]) handle[F[_]: Functor, M[_], N[_], A](
    fu: F[Unit], 
    ff: ([X] => F[M[X]]) ~> ([X] => N[F[X]])
  ): H[N, N[F[A]]]
}

object Effect {
  def apply[H[_[_], _]] given (ev: Effect[H]) = ev
}
