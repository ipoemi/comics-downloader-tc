package ipoemi.comicsdownloader.service

import java.io.{File => JFile}

import better.files._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import cats.{Applicative, Monad, Traverse}
import cats.instances.list._
import ipoemi.comicsdownloader.util._

trait ComicsDownloadService[N[_], F[_], A] {
  def read(a: N[A]): F[N[String]]

  def download(a: N[A]): F[File]

  def parseBooks(a: N[String]): F[N[List[N[A]]]]

  def parsePages(a: N[String]): F[N[List[N[A]]]]

  def zipTo(dir: File, target: File)(implicit m: Applicative[F]): F[File] = {
    dir.createIfNotExists()
    dir.zipTo(target).pure[F]
  }

  def downloadComics(a: N[A])(implicit m: Monad[F], na: NamedContent[N], tn: Traverse[N]): F[List[File]] = {
    for {
      content <- read(a)
      bookSites <- parseBooks(content).map(na.content)
      bookContents <- bookSites.traverse(read)
      pageSitess <- bookContents.traverse(parsePages)
      dirs <- pageSitess.traverse(downloadPages)
      zipFiles <- dirs.traverse(x => zipTo(na.content(x), (na.name(x) + ".zip").toFile))
    } yield zipFiles
  }

  def downloadPages(a: N[List[N[A]]])(implicit mf: Monad[F], tn: Traverse[N]): F[N[File]] = {
    a.map(_.traverse(download).map(files => files.head.parent)).sequence
  }
}

object ComicsDownloadService {
  def apply[N[_], F[_], A](implicit ev: ComicsDownloadService[N, F, A]) = ev

  import ipoemi.comicsdownloader.IO

  implicit def urlComicsDownloadService[N[_], A](
    implicit
    nc: NamedContent[N],
    cp: ContentParser[N, A, IO],
    cr: ContentReader[N, A, IO]
  ) = new ComicsDownloadService[N, IO, A] {
    def read(a: N[A]): IO[N[String]] = cr.read(a)

    def download(a: N[A]): IO[File] = cr.download(a)

    def parseBooks(a: N[String]): IO[N[List[N[A]]]] = cp.parseBooks(a)

    def parsePages(a: N[String]): IO[N[List[N[A]]]] = cp.parsePages(a)
  }
}
