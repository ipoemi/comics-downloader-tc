package ipoemi.comicsdownloader.util

import java.io.{File => JFile}

import better.files._

trait Readable[E[_], C[_], A] {
  def read(ca: C[A]): E[C[String]]
  def download(ca: C[A], fileName: String): E[C[File]]
}

object Readable {
  def apply[E[_], C[_], A](implicit R: Readable[E, C, A]): Readable[E, C, A] = R

  trait Syntax {

    implicit class ReadableOps[E[_], C[_], A](ca: C[A]) {
      def read(implicit R: Readable[E, C, A]): E[C[String]] =
        R.read(ca)

      def download(fileName: String)(implicit R: Readable[E, C, A]): E[C[File]] =
        R.download(ca, fileName)
    }

  }

}

object ReadableSyntax extends Readable.Syntax
