package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


sealed trait Writer[W, M[_], A]
case class Tell[W, M[_], A](stuff: W, wtf: A) extends Writer[W, M, A]
case class Listen[W, M[_], A, B](scope: M[B], wtf: W => B => A) extends Writer[W, M, A]
case class Censor[W, M[_], A, B](mod: W => W, scope: M[B], wtf: B => A) extends Writer[W, M, A]
