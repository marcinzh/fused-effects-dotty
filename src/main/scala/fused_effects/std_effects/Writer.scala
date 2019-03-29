package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


sealed trait Writer[W, M[_], A]
case class Tell[W, M[_], A](stuff: W, wtf: A) extends Writer[W, M, A]
case class Listen[W, M[_], A, B](scope: M[B], wtf: W => B => A) extends Writer[W, M, A]
case class Censor[W, M[_], A, B](mod: W => W, scope: M[B], wtf: B => A) extends Writer[W, M, A]

object Writer {
  type Ap1[W] = [M[_], A] => Writer[W, M, A]
  type Ap2[W, M[_]] = [A] => Writer[W, M, A]
}


implied Writer_Effect[W] for Effect[[M[_], A] => Writer[W, M, A]] {
  private type H = ThisHFunctor

  def (h: H[M, A]) fmap[M[_], A, B](f: A => B): H[M, B] = h match {
    case Tell(w, wtf) => Tell(w, f(wtf))
    case Listen(scope, wtf) => Listen(scope, w => wtf(w).andThen(f))
    case Censor(mod, scope, wtf) => Censor(mod, scope, wtf.andThen(f))
  }

  def (h: H[M, A]) hmap[M[_], N[_], A](ff: M ~> N): H[N, A] = h match {
    case Tell(w, wtf) => Tell(w, wtf)
    case Listen(scope, wtf) => Listen(ff(scope), wtf)
    case Censor(mod, scope, wtf) => Censor(mod, ff(scope), wtf)
  }

  def (h: H[M, M[A]]) handle[F[_] : Functor, M[_], N[_], A](
    fu: F[Unit],
    ff: ([X] => F[M[X]]) ~> ([X] => N[F[X]])
  ): H[N, N[F[A]]] = h match {
    case Tell(w, wtf) => Tell(w, ff(fu.mapConst(wtf)))
    case Listen(scope: M[tB], wtf) =>
      val scope2 = ff(fu.mapConst(scope))
      val wtf2 = (w: W) => (fb: F[tB]) => ff(fb.map(wtf(w)))
      Listen(scope2, wtf2)
    case Censor(mod, scope: M[tB], wtf) =>
      val scope2 = ff(fu.mapConst(scope))
      val wtf2 = (fb: F[tB]) => ff(fb.map(wtf))
      Censor(mod, scope2, wtf2)
  }
}

def tell[H[_[_], _], M[_], W](w: W) given (evM: Member[Writer.Ap1[W], H], evC: Carrier[H, M]): M[Unit] =
  send[Writer.Ap1[W]](Tell(w, evC.theMonad.pure(())))

def listen[H[_[_], _], M[_], W, A](scope: M[A]) given (evM: Member[Writer.Ap1[W], H], evC: Carrier[H, M]): M[(W, A)] =
  send[Writer.Ap1[W]](Listen(scope, w => a => evC.theMonad.pure((w, a))))

def listens[H[_[_], _], M[_], W, A, B](f: W => B)(scope: M[A]) given (evM: Member[Writer.Ap1[W], H], evC: Carrier[H, M]): M[(B, A)] =
  send[Writer.Ap1[W]](Listen(scope, w => a => evC.theMonad.pure((f(w), a))))

def censor[H[_[_], _], M[_], W, A](f: W => W)(scope: M[A]) given (evM: Member[Writer.Ap1[W], H], evC: Carrier[H, M]): M[A] =
  send[Writer.Ap1[W]](Censor(f, scope, evC.theMonad.pure(_)))
