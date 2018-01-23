package ipoemi.comicsdownloader.downloader

import java.io.{File => JFile}

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import better.files._
import simulacrum._

import scala.concurrent.{ExecutionContextExecutor, Future}

@typeclass
trait Compressor[A] {
  type F[_]
  def compress(a: A)(destFile: String): F[File]
}

object Compressor {
  def create[G[_], A](fn: A => String => G[File]) = new Compressor[A] {
    type F[B] = G[B]
    def compress(a: A)(destFile: String): F[File] = fn(a)(destFile)
  }

  implicit def fileCompressor(
    implicit
    ec: ExecutionContextExecutor,
    as: ActorSystem,
    materializer: ActorMaterializer
  ) = create[Future, File] { file =>
    (destFile: String) => Future(file.zipTo(destFile.toFile))
  }
}
