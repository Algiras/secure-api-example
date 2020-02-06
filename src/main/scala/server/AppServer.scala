package server

import cats.data.Kleisli
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import config.Config
import domain._
import server.UserStore.User
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import pureconfig.generic.auto._
import pureconfig.module.catseffect._
import tsec.authentication.{BearerTokenAuthenticator, SecuredRequestHandler, TSecAuthService, _}
import tsec.common.SecureRandomId
import tasks._

import scala.concurrent.duration._

object AppServer extends IOApp {
  val publicRoutes = HttpRoutes.of[IO] {
    case GET -> Root / "non-safe-resource" => Ok()
  }

  val privateRoutes = TSecAuthService[User, TSecBearerToken[UserId], IO] {
    case GET -> Root / "safe-resource" asAuthed user => Ok(s"Hello ${user.username}")
  }

  def routes(
      userCredentialStore: UserStore,
      tokenStore: BackingStore[IO, SecureRandomId, TSecBearerToken[UserId]],
      taskService: tasks.TaskService[IO]): Kleisli[IO, Request[IO], Response[IO]] = {

    val authenticator = BearerTokenAuthenticator[IO, UserId, User](
      tokenStore,
      userCredentialStore.identityStore,
      TSecTokenSettings(
        expiryDuration = 10.minute,
        maxIdle = None
      ))

    val secureRoutes = SecuredRequestHandler(authenticator).liftService(
      tasks.taskRoutes(tasks.UserTaskService(_, taskService)) <+> privateRoutes
    )
    val loginRoutes = login.loginRoute(authenticator, userCredentialStore.checkPassword)

    (publicRoutes <+> loginRoutes <+> secureRoutes).orNotFound
  }

  def run(args: List[String]): IO[ExitCode] =
    for {
      config <- loadConfigF[IO, Config]
      userCredentialStore <- UserStore(config.users.head, config.users.tail: _*)
      tokenStore <- TokenStore.empty
      taskService <- TaskService.empty
      httpServer = routes(userCredentialStore, tokenStore, taskService)
      loggingMiddleware = Logger.httpApp[IO](logHeaders = true, logBody = true)(_)
      res <- BlazeServerBuilder[IO]
        .bindHttp(host = config.http.host, port = config.http.port.getOrElse(0))
        .withHttpApp(loggingMiddleware(httpServer))
        .serve
        .compile
        .drain
        .map(_ => ExitCode.Success)
        .recover { case _ => ExitCode.Error }
    } yield res
}
