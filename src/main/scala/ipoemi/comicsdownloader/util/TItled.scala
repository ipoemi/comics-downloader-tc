package ipoemi.comicsdownloader.util

import simulacrum._

import cats.Traverse

@typeclass
trait Titled[F[_]] extends Traverse[F] {
  def title[A](fa: F[A]): String
  def value[A](fa: F[A]): A
}

object TitledSyntax extends Titled.ToTitledOps
