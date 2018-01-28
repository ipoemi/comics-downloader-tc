package ipoemi.comicsdownloader.util

import simulacrum._

import cats.{Applicative, Eval, Functor, Traverse, Id}
import cats.syntax.functor._

@typeclass
trait NamedContent[F[_]] {
  def name[A](fa: F[A]): String
  def content[A](fa: F[A]): A
}

object NamedContentSyntax extends NamedContent.ToNamedContentOps

object NamedContentInstances {
  implicit def namedContentTraverse[F[_], A](implicit nc: NamedContent[F], fc: Functor[F]) =
    new Traverse[F] {
      def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit ev: Applicative[G]): G[F[B]] =
        ev.map(f(nc.content(fa)))(c => fa.map(_ => c))

      def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
        f(b, nc.content(fa))

      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        f(nc.content(fa), lb)
    }
}
