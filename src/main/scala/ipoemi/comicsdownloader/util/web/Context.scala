package ipoemi.comicsdownloader.util.web

import cats.Traverse
import ipoemi.comicsdownloader.util.Titled
import simulacrum._

@typeclass
trait Context[F[_]] extends Titled[F] {
  def session[A](fa: F[A]): Session
  def leftMap[A](fa: F[A])(f1: Session => Session): F[A]
  def bimap[A, B](fa: F[A])(f1: Session => Session, f2: A => B): F[B]
}

object ContextSyntax extends Context.ToContextOps
