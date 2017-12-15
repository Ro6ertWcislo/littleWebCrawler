import org.htmlcleaner.TagNode

import scala.concurrent.{ Future, blocking}
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

object WebCrawler extends App {

  import org.htmlcleaner.HtmlCleaner
  import java.net.URL


  def crawlSeq(url: String, cleaner: HtmlCleaner, iter: Int): Iterable[String] = {
    try {
      val rootNode: TagNode = cleaner.clean(new URL(url))
      val urls: Iterable[String] = rootNode.getElementsByName("a", true)
        .map { elem => Option(elem.getAttributeByName("href")) }
        .filter(_.isDefined)
        .map(_.get)
        .toSet
        .filter(_.startsWith("https://www.bloomberg.com"))
      if (iter == 0) urls.toSet.take(2)
      else urls.flatMap(crawlSeq(_, cleaner, iter - 1)).toSet ++urls
    }

    catch {
      case ex: java.net.MalformedURLException => Set()
      case ex: java.net.UnknownServiceException => Set()
      case ex: java.io.IOException => Set()
    }
  }

  def timeMeasure[R](fun: => R): R = {
    val start = System.nanoTime()
    val res = fun
    val end = System.nanoTime()
    println("Execution lasted " + (end - start) / 1000000 + " ms")
    res
  }




 def fetchUrls(url: String,cleaner: HtmlCleaner, singleWebsite: Option[String] = None) : Future[Iterable[String]] = Future {
    val rootNode: TagNode = cleaner.clean(new URL(url))
    rootNode.getElementsByName("a", true)
      .map { elem => Option(elem.getAttributeByName("href")) }
      .filter(_.isDefined)
      .map(_.get)
      .toSet
      .filter(url => singleWebsite match {
        case None => true
        case Some(website) => url.startsWith(website)
      })
  }



  def crawlPar(url: String, cleaner: HtmlCleaner, iter: Int): Future[Iterable[String]] = {
    try {

      val urls: Future[Iterable[String]] = fetchUrls(url,cleaner,singleWebsite = Option("https://www.bloomberg.com"))

      if (iter == 0) return urls.map(_.toSet.take(2))

      val nestedFutures: Future[Iterable[Future[Iterable[String]]]]= urls.map(url => url.map(crawlPar(_,cleaner,iter-1)))

      val flattenedFutures: Future[Iterable[String]] = nestedFutures.flatMap(_.foldLeft(Future(Iterable.empty[String])){
        (previousFuture, next) =>
          for {

            previousResults ← previousFuture
            next ← next
          } yield previousResults ++ next
      })

      val result = for {
        parents <- urls
        children <- flattenedFutures
      }yield parents++children
      result.map(_.toSet)
    }

    catch {
      case ex: java.net.MalformedURLException => Future(Set())
      case ex: java.net.UnknownServiceException => Future(Set())
      case ex: java.io.IOException => Future(Set())
    }
  }


  val url = "https://www.bloomberg.com/company/london/?utm_source=bloomberg-menu&amp;utm_medium=bcom"


  val cleaner = new HtmlCleaner
  val props = cleaner.getProperties


  val i2 = timeMeasure {
    crawlSeq(url, cleaner, 2)
  }
  println(i2.size)
  println(i2)
  val start = System.nanoTime()
  val i: Future[Iterable[String]] = crawlPar(url, cleaner, 2)

  i.onComplete({
    case Success(value) =>
      val end = System.nanoTime()
      println("crawl2 lasted: " + (end - start) / 1000000 + "ms")
      println(value.size)
      println(value)
    case Failure(e) => e.printStackTrace()
  })


  Thread.sleep(1800000)
  println("koooooooooonieeeeeeeeeec")

}
