package fused_effects.abstraction
import mwords._


trait HFunctor[H[_[_], _]] {
  type ThisHFunctor = H

  def (h: H[M, A]) fmap[M[_], A, B](f: A => B): H[M, B]
  def (h: H[M, A]) hmap[M[_], N[_], A](ff: M ~> N): H[N, A]
}

object HFunctor {
  def apply[H[_[_], _]] given (ev: HFunctor[H]) = ev
}


def (h: H[F, F[A]]) handlePure[H[_[_], _] : HFunctor, F[_], G[_], A](ff: F ~> G): H[G, G[A]] =
  h.fmap(ff(_)).hmap(ff)
