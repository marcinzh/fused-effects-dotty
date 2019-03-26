package fused_effects.abstraction
import mwords._


trait Carrier[H[_[_], _], M[_]] extends HFunctor[H] with Monad[M] {
  type ThisH = H
  type ThisM = M

  def eff[A](wtf: H[M, M[A]]): M[A]
}


object Carrier {
  def apply[H[_[_], _], M[_]] given (ev: Carrier[H, M]) = ev
}
