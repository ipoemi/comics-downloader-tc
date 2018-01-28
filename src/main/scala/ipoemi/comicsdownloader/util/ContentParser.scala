package ipoemi.comicsdownloader.util

trait ContentParser[F[_], A] {
  def parseBooks(s: String): List[F[A]]
  def parsePages(s: String): List[F[A]]
}

object ContentParser {
  def apply[N[_], A](implicit ev: ContentParser[N, A]) = ev
}

object ContentParserSyntax {
  implicit class ContentParserOps[F[_], A](s: String) {
    def parseBooks(implicit ev: ContentParser[F, A]) = ev.parseBooks(s)
    def parsePages(implicit ev: ContentParser[F, A]) = ev.parsePages(s)
  }
}
