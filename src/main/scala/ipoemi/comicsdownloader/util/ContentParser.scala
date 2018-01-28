package ipoemi.comicsdownloader.util

import ipoemi.comicsdownloader.IO

trait ContentParser[N[_], A, F[_]] {
  def parseBooks(s: N[String]): F[N[List[N[A]]]]
  def parsePages(s: N[String]): F[N[List[N[A]]]]
}

object ContentParser {
  def apply[N[_], A, F[_]](implicit ev: ContentParser[N, A, F]) = ev
}

object ContentParserSyntax {
  implicit class ContentParserOps[N[_], A, F[_]](ns: N[String]) {
    def parseBooks(implicit ev: ContentParser[N, A, F]) = ev.parseBooks(ns)
    def parsePages(implicit ev: ContentParser[N, A, F]) = ev.parsePages(ns)
  }
}
