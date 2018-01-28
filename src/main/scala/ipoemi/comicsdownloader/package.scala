package ipoemi

import scala.concurrent.Future

package object comicsdownloader {
  type IO[A] = Future[A]
}
