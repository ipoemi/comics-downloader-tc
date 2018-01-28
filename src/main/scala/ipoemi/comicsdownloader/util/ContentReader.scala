package ipoemi.comicsdownloader.util

import java.io.{File => JFile}
import java.net.{URL, URLEncoder}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import akka.util.ByteString
import better.files._


trait ContentReader[A, F[_]] {
  def read(a: A): F[String]
  def download(a: A, fileName: String): F[File]
}

object ContentReader {
  def apply[A, F[_]](implicit ev: ContentReader[A, F]) = ev
}

object ContentReaderSyntax {

  implicit class ContentReaderOps[A, F[_]](a: A) {
    def read(implicit ev: ContentReader[A, F]) = ev.read(a)

    def download(fileName: String)(implicit ev: ContentReader[A, F]) = ev.download(a, fileName)
  }

}

object ContentReaderInstances {

  import ToUrlSyntax._
  import cats.{Traverse, Applicative}
  import cats.syntax.functor._
  import cats.syntax.traverse._
  import cats.syntax.applicative._

  implicit def webContentReader[A](
    implicit
    tr: ToUrl[A],
    ac: ActorSystem,
    am: Materializer,
    ec: ExecutionContext,
    io: Applicative[Future]
  ) = new ContentReader[A, Future] {

    def read(a: A): Future[String] = {
      for {
        r <- requestUrl(a.toUrl)
        s <- stringFromResponse(r)
      } yield s
    }

    def download(a: A, fileName: String): Future[File] = {
      val destFile = fileName.toFile
      for {
        r <- requestUrl(a.toUrl)
        s <- processResponse(r) { bs =>
          destFile.appendByteArray(bs.toArray)
        }
      } yield destFile
    }

    def requestUrl(
      url: URL, method: HttpMethod = HttpMethods.GET,
      headers: List[HttpHeader] = List(),
      entity: RequestEntity = HttpEntity.Empty
    ): Future[HttpResponse] = {
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

    def processRedirect(response: HttpResponse, httpMethod: HttpMethod): Future[HttpResponse] =
      if (response.status.intValue >= 300 && response.status.intValue < 400)
        for {
          url <- (new URL(response.getHeader("Location").get.value)).pure[Future]
          response <- requestUrl(url, httpMethod)
        } yield response
      else
        response.pure[Future]

    def stringFromResponse(response: HttpResponse): Future[String] = {
      response.entity.dataBytes.runFold("")(_ + _.decodeString("utf-8"))
    }

    def processResponse(response: HttpResponse)(f: ByteString => Unit): Future[Done] = {
      response.entity.dataBytes.runForeach(f)
    }
  }
}
