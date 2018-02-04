package ipoemi.comicsdownloader.util.web

import java.net.URL

import akka.http.scaladsl.model.HttpMethod
import akka.http.scaladsl.model.headers._

final case class Session(
  lastUrl: URL,
  cookies: Set[`Set-Cookie`],
  method: HttpMethod
)
