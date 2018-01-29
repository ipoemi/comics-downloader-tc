package ipoemi.comicsdownloader.util

trait ContentParser[F[_], A] {
  def parseBooks(s: String): List[F[A]]
  def parsePages(s: String): List[F[A]]
}

object ContentParser {
  def apply[N[_], A](implicit C: ContentParser[N, A]): ContentParser[N, A] = C

  trait Syntax {
    implicit class ContentParserOps[F[_], A](s: String) {
      def parseBooks(implicit C: ContentParser[F, A]): List[F[A]] = C.parseBooks(s)
      def parsePages(implicit C: ContentParser[F, A]): List[F[A]] = C.parsePages(s)
    }
  }

}

object ContentParserSyntax extends ContentParser.Syntax

