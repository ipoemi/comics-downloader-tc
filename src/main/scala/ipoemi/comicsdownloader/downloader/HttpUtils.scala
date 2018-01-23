package ipoemi.comicsdownloader.downloader

import java.io.{File => JFile}
import java.net.{URL, URLEncoder}

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.util.ByteString
import better.files._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

abstract class HttpUtils {
  implicit val executionContext: ExecutionContextExecutor
  implicit val materializer: ActorMaterializer
  implicit val actorSystem: ActorSystem

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
        url <- Future(new URL(response.getHeader("Location").get.value))
        response <- requestUrl(url, httpMethod)
      } yield response
    else
      Future.successful(response)

  def stringFromResponse(response: HttpResponse): Future[String] = {
    response.entity.dataBytes.runFold("")(_ + _.decodeString("utf-8"))
  }

  def processResponse[A](response: HttpResponse)(f: ByteString => Unit): Future[Done] = {
    response.entity.dataBytes.runForeach(f)
  }

  def downloadTo(response: HttpResponse, destPath: String): Future[File] = {
    def isValidFile(file: File, response: HttpResponse) = {
      val contentLength = response.entity.contentLengthOption.getOrElse(0L)
      file.notExists() || file.toJava.length() < contentLength
    }

    def newFile(destFile: File) = Future {
      destFile.createIfNotExists(createParents = true)
    }

    val destFile = destPath.toFile
    if (isValidFile(destFile, response)) {
      for {
        _ <- newFile(destFile)
        _ <- processResponse(response)(x => destFile.appendByteArray(x.toArray))
      } yield destFile
    } else {
      Future.successful(destFile)
    }
  }
}

object HttpUtils {
  def apply()(
    implicit _actorSystem: ActorSystem,
    _materializer: ActorMaterializer,
    _executionContext: ExecutionContextExecutor
  ): HttpUtils = new HttpUtils {
    implicit val executionContext: ExecutionContextExecutor = _executionContext
    implicit val materializer: ActorMaterializer = _materializer
    implicit val actorSystem: ActorSystem = _actorSystem
  }

}
