package ipoemi.comicsdownloader.model

import ipoemi.comicsdownloader.util._

case class Zangsisi[A](site: A)

object ZangsisiInstances {
  implicit def zangsisiContentParser[A, F[_]](implicit ev: ToUrl[A]) =
    new ContentParser[Zangsisi, A, F] {
      def parseBooks(s: Zangsisi[String]): F[Zangsisi[List[Zangsisi[A]]]] = ???
      def parsePages(s: Zangsisi[String]): F[Zangsisi[List[Zangsisi[A]]]] = ???
    }
}
