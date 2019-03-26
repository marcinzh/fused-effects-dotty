package mwords


trait Semigroup[T] {
  final type ThisSemigroup[T] = T

  def (a: T)|@|(b: T): T
}

object Semigroup {
  def apply[T] given (ev: Semigroup[T]) = ev
}


trait Monoid[T] extends Semigroup[T] {
  final type ThisMonoid[T] = T

  def empty: T
}

object Monoid {
  def apply[T] given (ev: Monoid[T]) = ev
}
