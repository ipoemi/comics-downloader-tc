package ipoemi.comicsdownloader.util

import java.net.URL

import simulacrum._

@typeclass
trait ToUrl[A] {
  def toUrl(a: A): URL
}

object ToUrlSyntax extends ToUrl.ToToUrlOps

object ToUrlInstances {
  implicit val stringToUrl: ToUrl[String] = new ToUrl[String] {
    def toUrl(a: String): URL = new URL(a)
  }
}
