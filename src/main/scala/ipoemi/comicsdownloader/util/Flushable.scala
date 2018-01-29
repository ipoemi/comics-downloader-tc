package ipoemi.comicsdownloader.util

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

import simulacrum._

@typeclass
trait Flushable[F[_]] {
  def flush[A](fa: F[A]): A
}

object Flushable {
  trait Syntax extends Flushable.ToFlushableOps
}

object FlushableSyntax extends Flushable.Syntax

object FlushableInstances {
  implicit val flushableFuture: Flushable[Future] = new Flushable[Future] {
    def flush[A](fa: Future[A]): A = Await.result(fa, Duration.Inf)
  }
}
