package ipoemi.comicsdownloader.service

import java.io.{File => JFile}

import better.files._
import cats._
import cats.implicits._
import ipoemi.comicsdownloader.util._
import ipoemi.comicsdownloader.util.web.Path

import scala.concurrent.Future

trait ComicsDownloadService[E[_], C[_], A] {

  import TitledSyntax._

  def read(ca: C[A])(implicit R: Readable[E, C, A]): E[C[String]]

  def download(ca: C[A], path: String)(implicit R: Readable[E, C, A]): E[C[File]]

  def parseBooks(cs: C[String])(implicit P: Parsable[C, A]): C[Vector[C[A]]]

  def parsePages(cs: C[String])(implicit P: Parsable[C, A]): C[Vector[C[A]]]

  def zipTo(dir: File, target: File)(implicit A: Applicative[E]): E[File] = {
    dir.createIfNotExists()
    dir.zipTo(target).pure[E]
  }

  def downloadComics(ca: C[A], path: String)(
    implicit
    M: Monad[E], T: Titled[C],
    P: Parsable[C, A], R: Readable[E, C, A],
    A: Awaitable[E]
  ): E[Vector[File]] = {
    for {
      _ <- path.toFile.createDirectories().pure[E]
      comics <- R.read(ca)
      books = parseBooks(comics).map(_.take(1)).value
      //_ = books.foreach(println)
      bookContents <- books.traverse(read)
      //_ = bookContents.foreach(println)
      booksPages = bookContents.map(parsePages)
      zipFiles <- booksPages.traverse(downloadPages(_, path))
    } yield zipFiles
  }

  def downloadPages(ca: C[Vector[C[A]]], path: String)(
    implicit M: Monad[E], R: Readable[E, C, A], T: Titled[C], A: Awaitable[E]
  ): E[File] = {
    val title = ca.title
    val dirPath = path + JFile.separator + title
    val zipFilePath = dirPath + ".zip"
    if (ca.value.size > 0)
      for {
        _ <- dirPath.toFile.createDirectories().pure[E]
        _ <- ca.value.traverse(x => download(x, dirPath + JFile.separator + x.title))
        file <- zipTo(dirPath.toFile, zipFilePath.toFile)
      } yield file
    else
      zipFilePath.toFile.pure[E]

  }
}

object ComicsDownloadService {
  def apply[E[_], C[_], A](implicit C: ComicsDownloadService[E, C, A]) = C

  import ParsableSyntax._
  import ReadableSyntax._

  implicit def webComicsDownloadService[C[_]] = new ComicsDownloadService[Future, C, web.Path] {
    def read(ca: C[web.Path])(implicit R: Readable[Future, C, Path]): Future[C[String]] = ca.read

    def download(ca: C[web.Path], path: String)(
      implicit R: Readable[Future, C, web.Path]
    ): Future[C[File]] = ca.download(path)

    def parseBooks(cs: C[String])(implicit P: Parsable[C, web.Path]): C[Vector[C[web.Path]]] =
      cs.parseBooks

    def parsePages(cs: C[String])(implicit P: Parsable[C, web.Path]): C[Vector[C[web.Path]]] =
      cs.parsePages
  }
}
