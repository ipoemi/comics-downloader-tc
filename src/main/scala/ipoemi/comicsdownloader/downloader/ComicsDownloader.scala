package ipoemi.comicsdownloader.downloader

import cats.Applicative
import cats.syntax.applicative._

class ComicsDownloader[F[_]: Applicative] {
  def getBooksLinks[A](a: A)(implicit cr: ContentReader[A]) = {
  }
}
