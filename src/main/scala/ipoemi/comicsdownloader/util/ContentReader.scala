package ipoemi.comicsdownloader.util

import java.io.{File => JFile}
import java.net.{URL, URLEncoder}

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import akka.util.ByteString
import better.files._
import ipoemi.comicsdownloader.IO

import scala.concurrent.ExecutionContext

trait ContentReader[N[_], A, F[_]] {
  def read(na: N[A]): F[N[String]]
  def download(na: N[A]): F[File]
}

object ContentReader {
  def apply[N[_], A, F[_]](implicit ev: ContentReader[N, A, F]) = ev
}

object ContentReaderSyntax {

  implicit class ContentReaderOps[N[_], F[_], A](na: N[A]) {
    def read(implicit ev: ContentReader[N, A, F]) = ev.read(na)

    def download(implicit ev: ContentReader[N, A, F]) = ev.download(na)
  }

}

object ContentReaderInstances {

  import NamedContentSyntax._
  import ToUrlSyntax._
  import cats.{Traverse, Applicative}
  import cats.instances.future._
  import cats.syntax.functor._
  import cats.syntax.traverse._
  import cats.syntax.applicative._

  implicit def webContentReader[N[_], A](
    implicit
    nc: NamedContent[N],
    tn: Traverse[N],
    tr: ToUrl[A],
    ac: ActorSystem,
    am: Materializer,
    ec: ExecutionContext,
    io: Applicative[IO]
  ) = new ContentReader[N, A, IO] {

    def read(na: N[A]): IO[N[String]] = {
      na.map { a =>
        for {
          r <- requestUrl(a.toUrl)
          s <- stringFromResponse(r)
        } yield s
      }.sequence
    }

    def download(na: N[A]): IO[File] = {
      val destFile = na.name.toFile
      for {
        r <- requestUrl(na.content.toUrl)
        s <- processResponse(r) { bs =>
          destFile.appendByteArray(bs.toArray)
        }
      } yield destFile
    }

    def requestUrl(
      url: URL, method: HttpMethod = HttpMethods.GET,
      headers: List[HttpHeader] = List(),
      entity: RequestEntity = HttpEntity.Empty
    ): IO[HttpResponse] = {
      val connection =
        if (url.getProtocol == "https")
          Http().outgoingConnectionHttps(url.getHost)
        else
          Http().outgoingConnection(url.getHost)

      val uriBuilder = new StringBuilder
      if (url.getPath == null || url.getPath == "")
        uriBuilder.append("/")
      else {
        uriBuilder.append(url.getPath.split("/").map(URLEncoder.encode(_, "utf-8")).mkString("/"))
        if (url.getPath.last == '/') uriBuilder.append("/")
      }

      if (url.getQuery != null) uriBuilder.append("?" + url.getQuery)

      val agent = RawHeader("User-Agent", "Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Safari/537.36")
      val defaultHeaders = if (headers.isEmpty) List(agent) else headers
      val req = HttpRequest(method = method, uri = uriBuilder.toString, entity = entity, headers = defaultHeaders)
      Source.single(req).via(connection).runWith(Sink.head).flatMap(processRedirect(_, method))
    }

    def processRedirect(response: HttpResponse, httpMethod: HttpMethod): IO[HttpResponse] =
      if (response.status.intValue >= 300 && response.status.intValue < 400)
        for {
          url <- (new URL(response.getHeader("Location").get.value)).pure[IO]
          response <- requestUrl(url, httpMethod)
        } yield response
      else
        response.pure[IO]

    def stringFromResponse(response: HttpResponse): IO[String] = {
      response.entity.dataBytes.runFold("")(_ + _.decodeString("utf-8"))
    }

    def processResponse(response: HttpResponse)(f: ByteString => Unit): IO[Done] = {
      response.entity.dataBytes.runForeach(f)
    }
  }
}
