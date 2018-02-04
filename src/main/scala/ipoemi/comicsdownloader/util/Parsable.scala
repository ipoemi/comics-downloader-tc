package ipoemi.comicsdownloader.util

trait Parsable[C[_], A] {
  def parseBooks(cs: C[String]): C[Vector[C[A]]]

  def parsePages(cs: C[String]): C[Vector[C[A]]]
}

object Parsable {
  def apply[C[_], A](implicit P: Parsable[C, A]): Parsable[C, A] = P

  trait Syntax {

    implicit class ParsableOps[C[_], A](cs: C[String]) {
      def parseBooks(implicit P: Parsable[C, A]): C[Vector[C[A]]] = P.parseBooks(cs)

      def parsePages(implicit P: Parsable[C, A]): C[Vector[C[A]]] = P.parsePages(cs)
    }

  }

}

object ParsableSyntax extends Parsable.Syntax

