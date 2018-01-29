package ipoemi.comicsdownloader.util

import simulacrum._

import cats.{Applicative, Eval, Functor, Traverse, Id}
import cats.syntax.functor._

@typeclass
trait NamedContent[F[_]] extends Traverse[F] {
  def name[A](fa: F[A]): String
  def content[A](fa: F[A]): A
}

object NamedContent {
  trait Syntax extends NamedContent.ToNamedContentOps
}

object NamedContentSyntax extends NamedContent.Syntax
