package ipoemi.comicsdownloader.util

import java.io.{File => JFile}
import java.net.{URL, URLEncoder}

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{RawHeader, `User-Agent`}
import akka.http.scaladsl.settings.ClientConnectionSettings
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.util.ByteString
import better.files._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}


trait ContentReader[A, F[_]] {
  def read(a: A): F[String]

  def download(a: A, fileName: String): F[File]
}

object ContentReader {
  def apply[A, F[_]](implicit C: ContentReader[A, F]): ContentReader[A, F] = C

  trait Syntax {
    implicit class ContentReaderOps[A, F[_]](a: A) {
      def read(implicit C: ContentReader[A, F]): F[String] =
        C.read(a)

      def download(fileName: String)(implicit C: ContentReader[A, F]): F[File] =
        C.download(a, fileName)
    }
  }
}

object ContentReaderSyntax extends ContentReader.Syntax

object ContentReaderInstances {

  import ToUrlSyntax._
  import cats.Applicative
  import cats.syntax.applicative._

  implicit def webContentReader[A](
    implicit
    tr: ToUrl[A],
    ac: ActorSystem,
    am: Materializer,
    ec: ExecutionContext,
    io: Applicative[Future]
  ): ContentReader[A, Future] = new ContentReader[A, Future] {

    def read(a: A): Future[String] = {
      for {
        r <- requestUrl(a.toUrl)
        s <- stringFromResponse(r)
      } yield s
    }

    def download(a: A, fileName: String): Future[File] = {
      val destFile = fileName.toFile
      for {
        _ <- Future.successful(destFile.createIfNotExists())
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
      var clientConnectionSettings =
        ClientConnectionSettings(ac.settings.config)
          .withIdleTimeout(15 seconds)
          .withConnectingTimeout(15 seconds)

      val connection =
        if (url.getProtocol == "https")
          Http().outgoingConnectionHttps(url.getHost, 443, Http().defaultClientHttpsContext, None, clientConnectionSettings)
        else
          Http().outgoingConnection(url.getHost, 80, None, clientConnectionSettings)

      val uriBuilder = new StringBuilder
      if (url.getPath == null || url.getPath == "")
        uriBuilder.append("/")
      else {
        uriBuilder.append(url.getPath.split("/").map(URLEncoder.encode(_, "utf-8")).mkString("/"))
        if (url.getPath.last == '/') uriBuilder.append("/")
      }

      if (url.getQuery != null) uriBuilder.append("?" + url.getQuery)

      val agent = `User-Agent`("Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Safari/537.36")
      val accept = RawHeader("Accept", "*/*")
      val defaultHeaders = if (headers.isEmpty) List(agent, accept) else headers
      val req = HttpRequest(method = method, uri = uriBuilder.toString, entity = entity, headers = defaultHeaders)
      retry(req, connection, 5).flatMap(processRedirect(_, method))
    }

    def retry(
      request: HttpRequest,
      connection: Flow[HttpRequest, HttpResponse, _],
      count: Int
    ): Future[HttpResponse] = {
      def responseFut = Source.single(request).via(connection).runWith(Sink.head)

      (0 until count).foldLeft(responseFut) { (fut, _) =>
        fut.recoverWith {
          case ex: Exception =>
            ac.log.error(ex.getMessage)
            responseFut
        }
      }.recover {
        case ex: Exception =>
          ac.log.error(ex.getMessage)
          HttpResponse(status = 404)
      }
    }

    def processRedirect(response: HttpResponse, httpMethod: HttpMethod): Future[HttpResponse] =
      if (response.status.intValue >= 300 && response.status.intValue < 400)
        for {
          url <- new URL(response.getHeader("Location").get.value).pure[Future]
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
