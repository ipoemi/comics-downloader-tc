package ipoemi.comicsdownloader.model

import java.net.URLEncoder

import cats._
import cats.implicits._
import ipoemi.comicsdownloader.util._
import ipoemi.comicsdownloader.util.web.Session
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model.Element

final case class Marumaru[A](session: web.Session, title: String, value: A)

object MarumaruInstances {

  implicit val webContextMarumaru = new web.Context[Marumaru] {
    def title[A](fa: Marumaru[A]): String = fa.title

    def session[A](fa: Marumaru[A]): Session = fa.session

    def value[A](fa: Marumaru[A]): A = fa.value

    def leftMap[A](fa: Marumaru[A])(f1: Session => Session): Marumaru[A] =
      fa.copy(session = f1(fa.session))

    override def map[A, B](fa: Marumaru[A])(f: A => B): Marumaru[B] =
      fa.copy(value = f(fa.value))

    def bimap[A, B](fa: Marumaru[A])(f1: Session => Session, f2: A => B): Marumaru[B] =
      fa.copy(session = f1(fa.session), value = f2(fa.value))

    def traverse[G[_], A, B](fa: Marumaru[A])(f: A => G[B])(implicit ev: Applicative[G]): G[Marumaru[B]] =
      f(fa.value).map(x => map(fa)(_ => x))

    def foldLeft[A, B](fa: Marumaru[A], b: B)(f: (B, A) => B): B = f(b, fa.value)

    def foldRight[A, B](fa: Marumaru[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      f(fa.value, lb)
  }

  implicit def marumaruParsable: Parsable[Marumaru, web.Path] =
    new Parsable[Marumaru, web.Path] {

      val imageExtensions = List("png", "jpg", "gif", "jpeg")

      def refineTitle(no: Int, title: String): String =
        s"${"%03d".format(no + 1)}.${title.replaceAll("[^ㄱ-ㅎ가-힣0-9a-zA-Z.\\-~ ]", "")}"

      def parseBooks(cs: Marumaru[String]): Marumaru[Vector[Marumaru[web.Path]]] = {
        cs.map { s =>
          val doc = JsoupBrowser().parseString(s)

          val aTags = (for {
            vContent <- doc >?> element("#vContent")
            as <- vContent >?> elementList("a")
          } yield as.toVector).getOrElse(Vector())

          aTags.zipWithIndex.map { case (aTag, i) =>
            Marumaru(cs.session, refineTitle(i, aTag.text), web.Path(aTag.attr("href")))
          }.toVector
        }
      }

      def parsePages(cs: Marumaru[String]): Marumaru[Vector[Marumaru[web.Path]]] = {
        cs.map { s =>
          val doc = JsoupBrowser().parseString(s)

          val imgTags = (for {
            primary <- doc >?> element(".gallery-template")
            imgList <- primary >?> elementList("img")
          } yield imgList).getOrElse(Vector())

          def isValidElem(elem: Element, srcAttrName: String) =
            elem.attrs.keySet.contains(srcAttrName) &&
              imageExtensions.exists(ext => elem.attrs(srcAttrName).toLowerCase.contains("." + ext))

          def mkLink(src: String, idx: Int) = {
            val ext = src.substring(src.lastIndexOf("."))
            Marumaru(cs.session, "%05d".format(idx) + ext, web.Path(src))
          }

          val links = imgTags.zipWithIndex map { case (elem, i) =>
            if (isValidElem(elem, "data-src")) {
              elem.attrs.get("data-src").map(mkLink(_, i))
            } else if (isValidElem(elem, "src")) {
              elem.attrs.get("src").map(mkLink(_, i))
            } else None
          }

          links.filter(_.nonEmpty).map(_.get).toVector
        }
      }

    }

}
