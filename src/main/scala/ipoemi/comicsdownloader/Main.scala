package ipoemi.comicsdownloader

import java.io.{File => JFile}

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import cats.instances.future._
import ipoemi.comicsdownloader.model.ZangsisiInstances._
import ipoemi.comicsdownloader.model._
import ipoemi.comicsdownloader.service._
import ipoemi.comicsdownloader.util.ContentReaderInstances._
import ipoemi.comicsdownloader.util.FlushableInstances._
import ipoemi.comicsdownloader.util.ToUrlInstances._

import scala.concurrent.{ExecutionContextExecutor, Future}

object Main extends App {

  implicit val actorSystem: ActorSystem = ActorSystem("myActorSystem")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher

  val downloader = ComicsDownloadService[Zangsisi, Future, String]
  for {
    _ <- downloader.downloadComics(
      Zangsisi("", "http://zangsisi.net/?page_id=123841"),
      "comics/신부이야기"
    ).recover {
      case err => err.printStackTrace()
    }
    _ <- Http().shutdownAllConnectionPools()
    _ <- actorSystem.terminate()
  } yield ()

}
