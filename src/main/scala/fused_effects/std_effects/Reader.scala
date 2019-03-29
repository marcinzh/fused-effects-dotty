package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


sealed trait Reader[E, M[_], A]
case class Ask[E, M[_], A](wtf: E => A) extends Reader[E, M, A]
case class Local[E, M[_], A, B](mod: E => E, scope: M[B], wtf: B => A) extends Reader[E, M, A]

object Reader {
  type Ap1[E] = [M[_], A] => Reader[E, M, A]
  type Ap2[E, M[_]] = [A] => Reader[E, M, A]
}


implied Reader_Effect[E] for Effect[Reader.Ap1[E]] {
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


def ask[H[_[_], _], M[_], E] given (evM: Member[Reader.Ap1[E], H], evC: Carrier[H, M]): M[E] =
  send[Reader.Ap1[E], H, M, E](Ask(evC.theMonad.pure(_)))

def asks[H[_[_], _], M[_], E, A](f: E => A) given (evM: Member[Reader.Ap1[E], H], evC: Carrier[H, M]): M[A] =
  send[Reader.Ap1[E], H, M, A](Ask(e => evC.theMonad.pure(f(e))))

def local[H[_[_], _], M[_], E, A](f: E => E, scope: M[A]) given (evM: Member[Reader.Ap1[E], H], evC: Carrier[H, M]): M[A] =
  send[Reader.Ap1[E], H, M, A](Local(f, scope, evC.theMonad.pure(_)))
