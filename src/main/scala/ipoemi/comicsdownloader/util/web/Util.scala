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
import cats.implicits._
import ipoemi.comicsdownloader.util.TitledSyntax._
import ipoemi.comicsdownloader.util.web.ContextSyntax._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

abstract class Util(
  implicit as: ActorSystem, ec: ExecutionContext, m: Materializer
) {

  type Connection = Flow[HttpRequest, HttpResponse, _]

  val clientConnectionSettings =
    ClientConnectionSettings(as.settings.config)
      .withIdleTimeout(15 seconds)
      .withConnectingTimeout(15 seconds)

  def pathToUrl(toPath: String, fromUrl: URL): URL = {
    val protocol = fromUrl.getProtocol
    val host = fromUrl.getHost
    val portStr = if (fromUrl.getPort == -1 || fromUrl.getPort == 80) "" else s":${fromUrl.getPort}"
    val fromPath = fromUrl.getPath

    val newPath = toPath
    if (toPath.startsWith("http")) {
      new URL(toPath)
    } else if (newPath.startsWith("/")) {
      new URL(s"$protocol://$host$portStr$newPath")
    } else {
      new URL(s"$protocol://$host$portStr$fromPath/$newPath")
    }
  }

  def requestUrl[F[_]](
    webPath: F[Path], method: HttpMethod,
    entity: RequestEntity = HttpEntity.Empty
  )(implicit C: Context[F]): Future[F[HttpResponse]] = {

    var url = pathToUrl(webPath.value.value, webPath.session.lastUrl)
    println("==============================Start==============================")
    println(s"url: ${url}")
    println("============================//Start==============================")

    val connection = if (url.getProtocol == "https")
      Http().outgoingConnectionHttps(
        url.getHost, 443, Http().defaultClientHttpsContext, None, clientConnectionSettings)
    else
      Http().outgoingConnection(url.getHost, 80, None, clientConnectionSettings)

    val uriBuilder = new StringBuilder
    if (url.getPath == null || url.getPath == "")
      uriBuilder.append("/")
    else {
      val newPath =
        url.getPath.split("/")
          .map(URLEncoder.encode(_, "CP949").replace("+", "%20"))
          .mkString("/")
      uriBuilder.append(newPath)
      if (url.getPath.last == '/') uriBuilder.append("/")
    }

    if (url.getQuery != null) uriBuilder.append("?" + url.getQuery)

    val agent = `User-Agent`("Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Safari/537.36")
    val accept = Accept(MediaRanges.`*/*`)
    //val headers = agent +: accept +: session.cookies.toVector
    val headers = agent +: accept +: Vector.empty
    val uri = uriBuilder.toString
    val newSession = webPath.session.copy(lastUrl = url)

    for {
      req <- Future(webPath.bimap(_ => newSession, _ => HttpRequest(method, uri, headers, entity)))
      res <- retry(req, connection)
      res1 <- processRedirect(res)
    } yield res1
  }

  def retry[F[_]](
    webRequest: F[HttpRequest],
    connection: Connection,
    count: Int = 5
  )(implicit C: Context[F]): Future[F[HttpResponse]] = {

    def responseFut = Source.single(webRequest.value).via(connection).runWith(Sink.head)

    (0 until count).foldLeft(responseFut) { (fut, _) =>
      fut.recoverWith {
        case ex: Exception =>
          as.log.error(ex.getMessage)
          responseFut
      }
    }.recover {
      case ex: Exception =>
        as.log.error(ex.getMessage)
        HttpResponse(status = 404)
    }.map(res => webRequest.map(_ => res))
  }

  def processRedirect[F[_]](
    webResponse: F[HttpResponse]
  )(implicit C: Context[F]): Future[F[HttpResponse]] = {
    val response = webResponse.value
    val session = webResponse.session
    val newCookies = response.headers.collect {
      case c: `Set-Cookie` => c
    } ++ session.cookies
    var newSession = session.copy(cookies = newCookies.toSet)

    if (response.status.intValue >= 300 && response.status.intValue < 400) {
      var newPath = response.getHeader("Location").get.value
      requestUrl(webResponse.bimap(_ => newSession, _ => Path(newPath)), session.method)
    } else {
      Future.successful(webResponse)
    }
  }

  def stringFromResponse[F[_]](
    webResponse: F[HttpResponse]
  )(implicit C: Context[F]): Future[F[String]] = {
    //as.log.debug(s"Start ${webResponse.session.lastUrl}")
    webResponse.traverse(_.entity.dataBytes.runFold("")(_ + _.decodeString("utf-8")))
  }

  def processResponse[F[_]](
    webResponse: F[HttpResponse]
  )(f: ByteString => Unit)(implicit C: Context[F]): Future[F[Done]] = {
    //as.log.debug(s"Start ${webResponse.session.lastUrl}")
    webResponse.traverse(_.entity.dataBytes.runForeach(f))
  }

}
