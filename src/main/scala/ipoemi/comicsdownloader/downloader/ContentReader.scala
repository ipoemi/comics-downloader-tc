package ipoemi.comicsdownloader.downloader

import simulacrum._
import cats.Monoid

@typeclass
trait ContentReader[A] {
  type F[_]
  def read(a: A): F[String]
}

object ContentReader {
  def create[G[_], A](fn: A => G[String]) = new ContentReader[A] {
    type F[B] = G[B]
    def read(a: A): F[String] = fn(a)
  }
}
