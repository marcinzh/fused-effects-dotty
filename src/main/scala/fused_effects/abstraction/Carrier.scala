package fused_effects.abstraction
import mwords._


// trait Carrier[H[_[_], _], M[_]] extends HFunctor[H] with Monad[M] {
// trait Carrier[H[_[_], _] : HFunctor, M[_] : Monad] {
trait Carrier[H[_[_], _], M[_]] {
  type ThisH = H
  type ThisM = M
  implicit def require_HFunctor: HFunctor[H]
  implicit def require_Monad: Monad[M]

  def eff[A](wtf: H[M, M[A]]): M[A]
}


object Carrier {
  def apply[H[_[_], _], M[_]] given (ev: Carrier[H, M]) = ev
}
