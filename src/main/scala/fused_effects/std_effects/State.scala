package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


sealed trait State[S, M[_], A]
case class Get[S, M[_], A](wtf: S => A) extends State[S, M, A]
case class Put[S, M[_], A](s: S, wtf: A) extends State[S, M, A]

object State {
  type Ap1[S] = [M[_], A] => State[S, M, A]
  type Ap2[S, M[_]] = [A] => State[S, M, A]
}


def get[H[_[_], _], M[_], S] given (evM: Member[State.Ap1[S], H], evC: Carrier[H, M]): M[S] =
  send[State.Ap1[S]](Get(evC.theMonad.pure(_)))

def put[H[_[_], _], M[_], S](s: S) given (evM: Member[State.Ap1[S], H], evC: Carrier[H, M]): M[Unit] =
  send[State.Ap1[S]](Put(s, evC.theMonad.pure(())))

def gets[H[_[_], _], M[_], S, A](f: S => A) given (evM: Member[State.Ap1[S], H], evC: Carrier[H, M]): M[A] = {
  import evC.theMonad
  get.map(f)
}

def modify[H[_[_], _], M[_], S](f: S => S) given (evM: Member[State.Ap1[S], H], evC: Carrier[H, M]): M[Unit] = {
  import evC.theMonad
  get.flatMap(s => put(f(s)))
}


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
