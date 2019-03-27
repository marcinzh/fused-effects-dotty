package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


case class ErrorC[E, M[_], A](run: M[Either[E, A]])


implied ErrorC_Monad[M[_] : Monad, E] for Monad[[A] => ErrorC[E, M, A]] {
  private type F = ThisMonad
  
  def pure[A](a: A): F[A] = ErrorC(Monad[M].pure(Right(a)))

  def (fa: F[A]) map[A, B](f: A => B): F[B] =
    ErrorC(fa.run.map {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    })

  def (fa: F[A]) flatMap[A, B](f: A => F[B]): F[B] =
    ErrorC(fa.run.flatMap {
      case Right(a) => f(a).run
      case Left(e) => Monad[M].pure(Left(e))
    })
}


implied Error_Carrier[H0[_[_], _] : Effect, M0[_], E] given (otherCarrier: Carrier[H0, M0]) for Carrier[
  ([M[_], A] => Error[E, M, A]) :+: H0,
  [A] => ErrorC[E, M0, A]
] {
  private type H = ThisH
  private type M = ThisM
  // import otherCarrier.{require_HFunctor => otherHFunctor}
  import otherCarrier.{require_Monad => otherMonad}
  implicit def require_HFunctor: HFunctor[ThisH] = the[HFunctor[ThisH]]
  implicit def require_Monad: Monad[ThisM] = the[Monad[ThisM]]

  def eff[A](h: H[M, M[A]]): M[A] = h match {
    case Sum.L(Throw(wtf)) => ErrorC(Monad[M0].pure(Left(wtf)))
    case Sum.L(Catch(scope: ErrorC[E, M0, tB], catcher, wtf)) =>
      ErrorC(scope.run.flatMap {
        case Right(b) => wtf(b).run
        case Left(e) => catcher(e).run.flatMap {
          case Right(b) => wtf(b).run
          case Left(e) => Monad[M0].pure(Left(e))
        }
      })
    case Sum.R(other) =>
      ErrorC {
        val ff = new ~>[[X] => Either[E, ErrorC[E, M0, X]], [X] => M0[Either[E, X]]] {
          def apply[A](e_carr: Either[E, ErrorC[E, M0, A]]) = e_carr match {
            case Right(carr) => carr.run
            case Left(e) => Monad[M0].pure(Left(e))
          }
        }
        val fu: Either[E, Unit] = Right(())
        val h0 = Effect[H0].handle(other)(fu, ff)
        otherCarrier.eff(h0)
      }
  }
}
