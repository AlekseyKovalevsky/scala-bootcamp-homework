package akovalevsky.scalabootcamp.homework.async

import java.net.URL
import java.util.concurrent.Executors
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source
import scala.util.Random

/**
 * Application:
 * - takes a web-page URL from arguments (args array)
 * - loads the web-page body, extracts HTTP links from it
 * - for all the found links, tries to fetch a server name header if there is one
 * - prints all the encountered unique server name values in alphabetical order
 *
 * Each link processing should be done in parallel.
 * Validation of arguments is not needed.
 *
 * Try to test it on http://google.com!
 */
object AsyncHomework extends App {
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  private val random = new Random()

  val distinctSortedServerNames = for {
    pageBody <- fetchPageBody(args(0) /*https://github.com*/)
    linkUrls <- findLinkUrls(pageBody)
    serverNames <- Future.traverse(linkUrls)(fetchServerName)
  } yield serverNames
    .collect({ case Some(name) => name })
    .map(_.toLowerCase)
    .distinct
    .sorted

  Await.result(distinctSortedServerNames, 15.seconds) foreach println

  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)
      try {
        source.mkString
      } finally {
        source.close()
      }
    }
  }

  private def fetchServerName(url: String): Future[Option[String]] = {
    println(s"Fetching server name header for $url")
    val future = Future {
      Option(new URL(url).openConnection().getHeaderField("Server"))
    }

    // this code helps to make sure that fetchServerName operations are executed in parallel
    // looking at the console output
    Thread.sleep(random.nextInt(150))
    future.onComplete(_ => println(s"Finished fetching server name header for $url"))

    future
  }

  private def findLinkUrls(html: String): Future[List[String]] = Future {
    val linkPattern = """href="(http[^"]+)"""".r
    linkPattern.findAllMatchIn(html).map(m => m.group(1)).toList
  }
}
