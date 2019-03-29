package fused_effects.abstraction
import mwords._


// trait Carrier[H[_[_], _], M[_]] extends HFunctor[H] with Monad[M] {
// trait Carrier[H[_[_], _] : HFunctor, M[_] : Monad] {
trait Carrier[H[_[_], _], M[_]] {
  type ThisH = H
  type ThisM = M
  implicit def theHFunctor: HFunctor[H]
  implicit def theMonad: Monad[M]

  def eff[A](wtf: H[M, M[A]]): M[A]
}

object Carrier {
  def apply[H[_[_], _], M[_]] given (ev: Carrier[H, M]) = ev
}


def send[H[_[_], _]] = new SendApply[H]

class SendApply[H[_[_], _]] {
  def apply[HH[_[_], _], M[_], A](h: H[M, M[A]]) given (evM: Member[H, HH], evC: Carrier[HH, M]): M[A] =
    evC.eff(evM.inj(h))
}
