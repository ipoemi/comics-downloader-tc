package ipoemi.comicsdownloader.service

import java.io.{File => JFile}

import scala.concurrent.Future
import scala.concurrent.ExecutionContextExecutor

import better.files._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import cats.{Applicative, Monad, Traverse}
import cats.instances.list._

import ipoemi.comicsdownloader.util._

trait ComicsDownloadService[N[_], F[_], A] {
  import cats.syntax.functor._
  import NamedContentSyntax._
  import FlushableSyntax._

  def read(a: N[A]): F[N[String]]

  def download(a: N[A], path: String): F[File]

  def parseBooks(a: String): List[N[A]]

  def parsePages(a: String): List[N[A]]

  def zipTo(dir: File, target: File)(implicit A: Applicative[F]): F[File] = {
    dir.createIfNotExists()
    dir.zipTo(target).pure[F]
  }

  def downloadComics(a: N[A], path: String)(
    implicit M: Monad[F], N: NamedContent[N], F: Flushable[F]
  ): F[List[File]] = {
    for {
      _ <- path.toFile.createDirectories().pure[F]
      comics <- read(a)
      bookSites = parseBooks(comics.content)
      bookContents <- bookSites.traverse(read)
      pageSitess = bookContents.map(_.map(parsePages))
      zipFiles = pageSitess.map(downloadPages(_, path))
    } yield zipFiles
  }

  def getPages(a: N[A])(
    implicit M: Monad[F], N: NamedContent[N], F: Flushable[F]
  ): N[List[N[A]]] = {
    read(a).map(_.map(parsePages)).flush
  }

  def downloadPages(a: N[List[N[A]]], path: String)(
    implicit M: Monad[F], N: NamedContent[N], F: Flushable[F]
  ): File = {
    val name = a.name
    val newPath = path + JFile.separator + name
    (for {
      _ <- newPath.toFile.createDirectories().pure[F]
      _ <- a.map(_.traverse(download(_, newPath)).map(_ => newPath.toFile)).sequence
      file <- zipTo(newPath.toFile, (newPath + ".zip").toFile)
    } yield file).flush
  }
}

object ComicsDownloadService {
  def apply[N[_], F[_], A](implicit C: ComicsDownloadService[N, F, A]) = C

  import cats.syntax.traverse._
  import cats.syntax.functor._
  import NamedContentSyntax._
  import ContentParserSyntax._
  import ContentReaderSyntax._

  implicit def urlComicsDownloadService[N[_], A](
    implicit
    A: Applicative[Future],
    N: NamedContent[N],
    T: ToUrl[A],
    CP: ContentParser[N, A],
    CR: ContentReader[A, Future]
  ) = new ComicsDownloadService[N, Future, A] {
    def read(na: N[A]): Future[N[String]] = na.map(_.read).sequence

    def download(na: N[A], path: String): Future[File] =
      (na.content).download(path + JFile.separator + na.name)

    def parseBooks(a: String): List[N[A]] = a.parseBooks

    def parsePages(a: String): List[N[A]] = a.parsePages
  }
}
