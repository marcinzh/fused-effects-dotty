package fused_effects.std_effects
import fused_effects.abstraction._
import mwords._


case class WriterC[W, M[_], A](run: StateC[W, M, A])
