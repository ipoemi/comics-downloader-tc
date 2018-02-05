package ipoemi.comicsdownloader

import java.io.{File => JFile}
import java.net.{URL, URLEncoder}

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.model.headers._
import akka.stream.ActorMaterializer
import cats.data._
import cats.instances.future._
import ipoemi.comicsdownloader.model.MarumaruInstances._
import ipoemi.comicsdownloader.model._
import ipoemi.comicsdownloader.service._
import ipoemi.comicsdownloader.util._
import ipoemi.comicsdownloader.util.web.Instances._
import ipoemi.comicsdownloader.util.AwaitableInstances._

import scala.concurrent.Future
import scala.concurrent.ExecutionContextExecutor

object Main extends App {

  implicit val actorSystem: ActorSystem = ActorSystem("myActorSystem")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher

  val comicsRoot = "http://marumaru.in/b/manga/133012"
  val session = web.Session(new URL(comicsRoot), Set.empty[`Set-Cookie`], HttpMethods.GET)
  val downloader = ComicsDownloadService[Future, Marumaru, web.Path]

  val program = for {
    _ <- downloader.downloadComics(
      Marumaru(session, "", web.Path(comicsRoot)),
      "comics/늑대와향신료"
    )
    _ <- Http().shutdownAllConnectionPools()
    _ <- actorSystem.terminate()
  } yield ()

}
