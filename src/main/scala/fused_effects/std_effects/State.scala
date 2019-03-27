package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


sealed trait State[S, M[_], A]
case class Get[S, M[_], A](wtf: S => A) extends State[S, M, A]
case class Put[S, M[_], A](s: S, wtf: A) extends State[S, M, A]

