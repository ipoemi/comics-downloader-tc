package ipoemi.comicsdownloader.downloader

import java.io.{File => JFile}
import better.files._
import simulacrum._

@typeclass
trait ContentDownloader[A] {
  type F[_]
  def download(a: A)(dir: String, fileName: String): F[File]
}

object ContentDownloader {
  def create[G[_], A](fn: A => (String, String) => G[File]) = new ContentDownloader[A] {
    type F[B] = G[B]
    def download(a: A)(dir: String, fileName: String): F[File] = fn(a)(dir, fileName)
  }
}
