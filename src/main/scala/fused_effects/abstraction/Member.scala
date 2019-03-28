package fused_effects.abstraction
import mwords._


trait Member[Sub[_[_], _], Super[_[_], _]] {
  final type ThisSub = Sub
  final type ThisSuper = Super

  def inj[M[_], A](h: Sub[M, A]): Super[M, A]
  def prj[M[_], A](h: Super[M, A]): Option[Sub[M, A]]
}

trait MemberLow1 {
  implied Member_Refl[H[_[_], _]] for Member[H, H] {
    private type Sub = ThisSub
    private type Super = ThisSuper

    def inj[M[_], A](h: Sub[M, A]): Super[M, A] = h
    def prj[M[_], A](h: Super[M, A]): Option[Sub[M, A]] = Some(h)
  }
}

trait MemberLow2 extends MemberLow1 {
  implied Member_R[H[_[_], _], J[_[_], _], HH[_[_], _]] given (ev: Member[H, HH]) for Member[H, (J :+: HH)] {
    private type Sub = ThisSub
    private type Super = ThisSuper

    def inj[M[_], A](h: Sub[M, A]): Super[M, A] = Sum.R(ev.inj(h))
    def prj[M[_], A](h: Super[M, A]): Option[Sub[M, A]] = h match {
      case Sum.R(hh) => ev.prj(hh)
      case _ => None
    }
  }
}

object Member extends MemberLow2 {
  implied Member_L[H[_[_], _], HH[_[_], _]] for Member[H, (H :+: HH)] {
    private type Sub = ThisSub
    private type Super = ThisSuper

    def inj[M[_], A](h: Sub[M, A]): Super[M, A] = Sum.L(h)
    def prj[M[_], A](h: Super[M, A]): Option[Sub[M, A]] = h match {
      case Sum.L(hh) => Some(hh)
      case _ => None
    }
  }
}

def send[H[_[_], _], HH[_[_], _], M[_], A](h: H[M, M[A]]) given (evM: Member[H, HH], evC: Carrier[HH, M]): M[A] =
  evC.eff(evM.inj(h))
