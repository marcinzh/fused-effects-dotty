package mwords


def the[A] given (ev: A) = ev


trait ~>[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
  def mono[A] = (fa: F[A]) => apply(fa)
}
