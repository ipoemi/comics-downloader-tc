package ipoemi.comicsdownloader.model

import ipoemi.comicsdownloader.util._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model.Element

case class Zangsisi[A](name: String, content: A)

object ZangsisiInstances {
  implicit def zangsisiContentParser =
    new ContentParser[Zangsisi, String] {

      val imageExtensions = List("png", "jpg", "gif", "jpeg")

      def refineTitle(no: Int, title: String): String =
        s"${"%03d".format(no + 1)}.${title.replaceAll("[^ㄱ-ㅎ가-힣0-9a-zA-Z.\\-~ ]", "")}"

      def parseBooks(s: String): List[Zangsisi[String]] = {
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
          Zangsisi(refineTitle(i, aTag.text), aTag.attr("href"))
        }.toList
      }

      def parsePages(s: String): List[Zangsisi[String]] = {
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

        def mkLink(src: String, idx: Int): Zangsisi[String] = {
          val ext = src.substring(src.lastIndexOf("."))
          Zangsisi("%05d".format(idx) + ext, src)
        }

        val links = imgTags.zipWithIndex map { case (elem, i) =>
          if (isValidElem(elem, "src")) {
            elem.attrs.get("src").map(mkLink(_, i))
          } else if (isValidElem(elem, "data-src")) {
            elem.attrs.get("data-src").map(mkLink(_, i))
          } else None
        }

        links.filter(_.nonEmpty).map(_.get).toList
      }
    }
}
