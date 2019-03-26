package fused_effects
import mwords._


trait HFunctor[H[_[_], _]] {
  type ThisHFunctor = H

  def (h: H[M, A]) fmap[M[_], A, B](f: A => B): H[M, B]
  def (h: H[M, A]) hmap[M[_], N[_], A](ff: M ~> N): H[N, A]
}

object HFunctor {
  def apply[H[_[_], _]] given (ev: HFunctor[H]) = ev
}
