package ipoemi.comicsdownloader.util.web

import java.net.{URL, URLEncoder}

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.settings.ClientConnectionSettings
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.util.ByteString
import better.files._
import cats.implicits._
import ContextSyntax._
import ipoemi.comicsdownloader.util.Readable

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

object Instances {

  implicit def webContextReadable[F[_]](
    implicit
    C: Context[F],
    actorSystem: ActorSystem,
    materializer: Materializer,
    executionContext: ExecutionContext
  ): Readable[Future, F, Path] = new Readable[Future, F, Path] {
    val util = new Util() {}

    def read(a: F[Path]): Future[F[String]] =
      for {
        res <- util.requestUrl(a, a.session.method)
        str <- util.stringFromResponse(res)
      } yield str

    def download(a: F[Path], fileName: String): Future[F[File]] =
      for {
        destFile <- fileName.toFile.createIfNotExists().pure[Future]
        res <- util.requestUrl(a, a.session.method)
        done <- util.processResponse(res)(bs => destFile.appendByteArray(bs.toArray))
      } yield done.map(_ => destFile)

  }

}
