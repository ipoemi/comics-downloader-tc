package ipoemi.comicsdownloader.model

import cats.syntax.functor._
import cats.{Applicative, Eval}
import ipoemi.comicsdownloader.util._
import ipoemi.comicsdownloader.util.web.Session
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model.Element

final case class Zangsisi[A](session: web.Session, title: String, value: A)

object ZangsisiInstances {

  implicit val webContextZangsisi: web.Context[Zangsisi] = new web.Context[Zangsisi] {

    def session[A](fa: Zangsisi[A]): Session = fa.session

    def leftMap[A](fa: Zangsisi[A])(f1: Session => Session): Zangsisi[A] =
      fa.copy(session = f1(fa.session))

    def bimap[A, B](fa: Zangsisi[A])(f1: Session => Session, f2: A => B): Zangsisi[B] =
      fa.copy(session = f1(fa.session), value = f2(fa.value))

    def title[A](fa: Zangsisi[A]): String = fa.title

    def value[A](fa: Zangsisi[A]): A = fa.value

    override def map[A, B](fa: Zangsisi[A])(f: A => B): Zangsisi[B] =
      fa.copy(value = f(fa.value))

    def traverse[G[_], A, B](fa: Zangsisi[A])(f: A => G[B])(implicit A: Applicative[G]): G[Zangsisi[B]] =
      f(fa.value).map(x => map(fa)(_ => x))

    def foldLeft[A, B](fa: Zangsisi[A], b: B)(f: (B, A) => B): B =
      f(b, fa.value)

    def foldRight[A, B](fa: Zangsisi[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      f(fa.value, lb)
  }

  implicit def parsableZangsisi: Parsable[Zangsisi, web.Path] =
    new Parsable[Zangsisi, web.Path] {

      def parseBooks(cs: Zangsisi[String]): Zangsisi[Vector[Zangsisi[web.Path]]] = {
        cs.map { s =>
          val doc = JsoupBrowser().parseString(s)

          val recentPostOpt = doc >?> element("#recent-post")
          val postOpt = doc >?> element("#post")

          val aTags =
            (for {
              recentPost <- recentPostOpt.orElse(postOpt)
              contents <- recentPost >?> element(".contents")
              aTagList <- contents >?> elementList("a")
            } yield aTagList.toVector).getOrElse(Vector())

          aTags.zipWithIndex.map { case (aTag, i) =>
            Zangsisi(cs.session, refineTitle(i, aTag.text), web.Path(aTag.attr("href")))
          }.toVector
        }
      }

      def parsePages(cs: Zangsisi[String]): Zangsisi[Vector[Zangsisi[web.Path]]] = {
        cs.map { s =>
          val doc = JsoupBrowser().parseString(s)

          val postOpt = (doc >?> element("#recent-post")).orElse(doc >?> element("#post"))

          val imgTagsOpt = postOpt match {
            case Some(post) =>
              for {
                contents <- post >?> element(".contents")
                imgTags <- contents >?> elementList("img")
              } yield imgTags.toVector

            case None =>
              for {
                mainOuter <- doc >?> element(".main-outer")
                contents <- mainOuter >?> element(".post-body")
                imgTags <- contents >?> elementList("img")
              } yield imgTags.toVector
          }

          val imgTags = imgTagsOpt.getOrElse(Vector())

          def isValidElem(elem: Element, srcAttrName: String) =
            elem.attrs.keySet.contains(srcAttrName) &&
              imageExtensions.exists(ext => elem.attrs(srcAttrName).toLowerCase.contains("." + ext))

          def mkLink(src: String, idx: Int) = {
            val ext = src.substring(src.lastIndexOf("."))
            Zangsisi(cs.session, "%05d".format(idx) + ext, web.Path(src))
          }

          val links = imgTags.zipWithIndex map { case (elem, i) =>
            if (isValidElem(elem, "src")) {
              elem.attrs.get("src").map(mkLink(_, i))
            } else if (isValidElem(elem, "data-src")) {
              elem.attrs.get("data-src").map(mkLink(_, i))
            } else None
          }

          links.filter(_.nonEmpty).map(_.get).toVector
        }
      }

      val imageExtensions = List("png", "jpg", "gif", "jpeg")

      def refineTitle(no: Int, title: String): String =
        s"${"%03d".format(no + 1)}.${title.replaceAll("[^ㄱ-ㅎ가-힣0-9a-zA-Z.\\-~ ]", "")}"
    }
}
