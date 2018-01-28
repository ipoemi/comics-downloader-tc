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
  def read(a: N[A]): F[N[String]]

  def download(a: N[A]): F[File]

  def parseBooks(a: String): List[N[A]]

  def parsePages(a: String): List[N[A]]

  def zipTo(dir: File, target: File)(implicit ev: Applicative[F]): F[File] = {
    dir.createIfNotExists()
    dir.zipTo(target).pure[F]
  }

  import NamedContentSyntax._

  def downloadComics(a: N[A])(
    implicit m: Monad[F], na: NamedContent[N], tn: Traverse[N]
  ): F[List[File]] = {
    for {
      comics <- read(a)
      bookSites = parseBooks(comics.content)
      bookContents <- bookSites.traverse(read)
      pageSitess = bookContents.map(_.map(parsePages))
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

  import cats.syntax.traverse._
  import cats.syntax.functor._
  import NamedContentSyntax._
  import ContentParserSyntax._
  import ContentReaderSyntax._

  implicit def urlComicsDownloadService[N[_], A](
    implicit
    tr: Traverse[N],
    ac: Applicative[Future],
    nc: NamedContent[N],
    tu: ToUrl[A],
    cp: ContentParser[N, A],
    cr: ContentReader[A, Future]
  ) = new ComicsDownloadService[N, Future, A] {
    def read(na: N[A]): Future[N[String]] = tr.sequence(na.map(cr.read(_)))

    def download(na: N[A]): Future[File] = cr.download(na.content, na.name)

    def parseBooks(a: String): List[N[A]] = a.parseBooks

    def parsePages(a: String): List[N[A]] = a.parsePages
  }
}
