package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


sealed trait Error[E, M[_], A]
case class Throw[E, M[_], A](wtf: E) extends Error[E, M, A]
case class Catch[E, M[_], A, B](scope: M[B], catcher: E => M[B], wtf: B => A) extends Error[E, M, A]

