package fused_effects.abstraction
import mwords._


enum Sum[F[_[_], _], G[_[_], _], M[_], A] {
  case L(run: F[M, A])
  case R(run: G[M, A])
}

type :+:[F[_[_], _], G[_[_], _]] = [M[_], A] => Sum[F, G, M, A]

object Sum {
  implied Sum_Effect[H1[_[_], _]: Effect, H2[_[_], _]: Effect] for Effect[[M[_], A] => Sum[H1, H2, M, A]] {
    private type H = H1 :+: H2

    def (h: H[M, A]) fmap[M[_], A, B](f: A => B): H[M, B] = h match {
      case Sum.L(h1) => Sum.L(h1.fmap(f))
      case Sum.R(h2) => Sum.R(h2.fmap(f))
    }

    def (h: H[M, A]) hmap[M[_], N[_], A](ff: M ~> N): H[N, A] = h match {
      case Sum.L(h1) => Sum.L(h1.hmap(ff))
      case Sum.R(h2) => Sum.R(h2.hmap(ff))
    }

    def (h: H[M, M[A]]) handle[F[_] : Functor, M[_], N[_], A](
      fu: F[Unit], 
      ff: ([X] => F[M[X]]) ~> ([X] => N[F[X]])
    ): H[N, N[F[A]]] = h match {
      case Sum.L(h1) => Sum.L(h1.handle(fu, ff))
      case Sum.R(h2) => Sum.R(h2.handle(fu, ff))
    }
  }
}
