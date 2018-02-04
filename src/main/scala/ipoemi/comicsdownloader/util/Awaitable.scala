package ipoemi.comicsdownloader.util

import simulacrum._

@typeclass
trait Awaitable[F[_]] {
  def await[A](fa: F[A]): F[A]
}

object Awaitable {
  trait Syntax extends Awaitable.ToAwaitableOps
}

object AwaitableSyntax extends Awaitable.Syntax

object AwaitableInstances {
  import scala.concurrent.{Future, Await}
  import scala.concurrent.duration._

  implicit val awaitableFuture = new Awaitable[Future] {
    def await[A](fa: Future[A]): Future[A] = Future.successful(Await.result(fa, Duration.Inf))
  }
}