package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


sealed trait Reader[E, M[_], A]
case class Ask[E, M[_], A](wtf : E => A) extends Reader[E, M, A]
case class Local[E, M[_], A, B](mod : E => E, scope : M[B], wtf : B => A) extends Reader[E, M, A]

case class ReaderC[E, M[_], A](run : E => M[A])
