package akovalevsky.scalabootcamp.homework.http

import akovalevsky.scalabootcamp.homework.effects.SharedStateHomework.Cache
import cats.data.Kleisli
import cats.effect.{ContextShift, ExitCode, IO, IOApp}
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.{HttpRoutes, Request, Response}
import org.http4s.dsl.io._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.implicits.{http4sKleisliResponseSyntaxOptionT, http4sLiteralsSyntax}

import java.util.UUID.randomUUID
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Random

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// The exact protocol and message format to use is not specified and should be designed while working on the task.
object GuessServer extends IOApp {

  import io.circe.generic.auto._
  import org.http4s.circe.CirceEntityCodec._
  import akovalevsky.scalabootcamp.homework.http.GuessServer.Protocol._

  object Protocol {

    sealed trait GuessTip

    object GuessTip {

      case object Smaller extends GuessTip

      case object Greater extends GuessTip

    }

    case class GuessGameSessionView(token: String, attemptsLeft: Int, message: String, tip: Option[GuessTip],
                                    gameEnded: Boolean, gameWon: Boolean)

    implicit class GuessGameSessionExtensions(guessGameSession: GuessGameSession) {
      def toView(message: String = "", tip: Option[GuessTip] = None): GuessGameSessionView =
        GuessGameSessionView(guessGameSession.token, guessGameSession.attemptsLeft, message, tip,
          guessGameSession.gameEnded, guessGameSession.gameWon)
    }

  }

  case class GuessGameSession(token: String, number: Int, attemptsLeft: Int, gameEnded: Boolean = false,
                              gameWon: Boolean = false)

  private val random = new Random()

  object MinNumberMatcher extends QueryParamDecoderMatcher[Int](name = "min")

  object MaxNumberMatcher extends QueryParamDecoderMatcher[Int](name = "max")

  object SessionTokenMatcher extends QueryParamDecoderMatcher[String](name = "token")

  object GuessMatcher extends QueryParamDecoderMatcher[Int](name = "guess")


  // RPC-style API
  private def routes(gameSessions: Cache[IO, String, GuessGameSession]) = HttpRoutes.of[IO] {
    case GET -> Root / "guessing-game" / "start" :? MinNumberMatcher(min) +& MaxNumberMatcher(max) =>
      val gameSession = for {
        sessionToken <- IO(randomUUID().toString)
        number <- IO(random.between(min, max))
        gameSession <- IO.pure(GuessGameSession(sessionToken, number, 10))
        _ <- gameSessions.put(sessionToken, gameSession)
      } yield gameSession

      gameSession.flatMap { gs =>
        Ok(gs.toView(message = s"The game started! You have ${gs.attemptsLeft} attempts"))
          .map(_.addCookie("guessSession", gs.token))
      }

    case GET -> Root / "guessing-game" / "try" :? SessionTokenMatcher(token) +& GuessMatcher(guess) =>
      for {
        gs <- gameSessions.get(token)
        response <- gs match {
          case None => BadRequest(s"The game with token $token not found")
          case Some(gs) if gs.gameEnded => BadRequest(s"The game is over")
          case Some(gs) if gs.number > guess =>
            val attemptsLeft = gs.attemptsLeft - 1
            val newGs = gs.copy(attemptsLeft = attemptsLeft, gameEnded = attemptsLeft == 0)
            gameSessions.put(token, newGs) *>
              Ok(newGs.toView(s"The number is greater than $guess", tip = Some(GuessTip.Greater)))
          case Some(gs) if gs.number < guess =>
            val attemptsLeft = gs.attemptsLeft - 1
            val newGs = gs.copy(attemptsLeft = attemptsLeft, gameEnded = attemptsLeft == 0)
            gameSessions.put(token, newGs) *>
              Ok(newGs.toView(s"The number is smaller than $guess", tip = Some(GuessTip.Smaller)))
          case Some(gs) =>
            val newGs = gs.copy(attemptsLeft = gs.attemptsLeft - 1, gameEnded = true, gameWon = true)
            gameSessions.put(token, newGs) *>
              Ok(newGs.toView(s"The number is $guess, you guessed right"))
        }
      } yield response

  }

  def httpApp(gameSessions: Cache[IO, String, GuessGameSession]): Kleisli[IO, Request[IO], Response[IO]] =
    routes(gameSessions).orNotFound

  override def run(args: List[String]): IO[ExitCode] = {
    (for {
      gameSessions <- Cache.of[IO, String, GuessGameSession](expiresIn = 1.hour, checkOnExpirationsEvery = 10.seconds)
      _ <- BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9001, host = "localhost")
        .withHttpApp(httpApp(gameSessions))
        .serve
        .compile
        .drain

    } yield ()) as ExitCode.Success
  }
}

object GuessClient extends IOApp {

  import io.circe.generic.auto._
  import org.http4s.circe.CirceEntityCodec._
  import akovalevsky.scalabootcamp.homework.http.GuessServer.Protocol._

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  private val guessingGameApi = uri"http://localhost:9001/guessing-game"

  def printLine(string: String): IO[Unit] = IO(println(string))

  override def run(args: List[String]): IO[ExitCode] = {
    BlazeClientBuilder[IO](ExecutionContext.global).resource.use { client =>
      def guess(gameToken: String, min: Int, max: Int): IO[Unit] = for {
        _ <- IO.sleep(3.seconds)
        mid <- IO.pure((min + max) / 2)
        _ <- printLine(s"Trying to guess: $mid")
        gameSession <- client.expect[GuessGameSessionView]((guessingGameApi / "try")
          .withQueryParam("token", gameToken)
          .withQueryParam("guess", mid))
        _ <- printLine(gameSession.message)
        _ <- printLine(s"You have ${gameSession.attemptsLeft} attempts left")
        _ <- gameSession match {
          case gs if gs.gameEnded => printLine(s"The game is over, you ${if (gameSession.gameWon) "won" else "lose"}")
          case gs if gs.tip.contains(GuessTip.Smaller) => guess(gameToken, min, mid - 1)
          case gs if gs.tip.contains(GuessTip.Greater) => guess(gameToken, mid + 1, max)
        }
      } yield ()

      for {
        _ <- printLine("Starting the game!")
        gameSessionInitial <- client.expect[GuessGameSessionView]((guessingGameApi / "start")
          .withQueryParam("min", 1)
          .withQueryParam("max", 100))
        _ <- printLine(gameSessionInitial.message)
        _ <- guess(gameSessionInitial.token, 1, 100)
      } yield ()
    } as ExitCode.Success
  }
}
