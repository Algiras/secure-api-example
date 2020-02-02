package server

import server.UserStore.User
import cats.data.Kleisli
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.http4s._
import org.http4s.circe.{jsonEncoderOf, jsonOf}
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import tsec.authentication.{BearerTokenAuthenticator, SecuredRequestHandler, TSecAuthService, _}
import tsec.common.SecureRandomId

import scala.concurrent.duration._

object AppServer extends IOApp {

  implicit val loginUserEncoder: Encoder[UserStore.UsernamePasswordCredentials] = deriveEncoder
  implicit val loginUserDecoder: Decoder[UserStore.UsernamePasswordCredentials] = deriveDecoder

  implicit val entityLoginUserEncoder: EntityEncoder[IO, UserStore.UsernamePasswordCredentials] =
    jsonEncoderOf[IO, UserStore.UsernamePasswordCredentials]
  implicit val entityLoginUserDecoder: EntityDecoder[IO, UserStore.UsernamePasswordCredentials] =
    jsonOf[IO, UserStore.UsernamePasswordCredentials]

  implicit val listS: EntityEncoder[IO, List[String]] = jsonEncoderOf[IO, List[String]]

  val settings: TSecTokenSettings = TSecTokenSettings(
    expiryDuration = 10.minute,
    maxIdle = None
  )

  def loginRoute(
      auth: BearerTokenAuthenticator[IO, UserStore.UserId, User],
      checkPassword: UserStore.UsernamePasswordCredentials => IO[Option[User]]): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case req @ POST -> Root / "login" =>
        (for {
          user <- req.as[UserStore.UsernamePasswordCredentials]
          userOpt <- checkPassword(user)
        } yield userOpt).flatMap {
          case Some(user) => auth.create(user.id).map(auth.embed(Response(Status.Ok), _))
          case None => IO.pure(Response[IO](Status.Unauthorized))
        }
    }

  val publicRoutes = HttpRoutes.of[IO] {
    case GET -> Root / "non-safe-resource" => Ok()
  }

  def privateRoutes(userTaskService: UserStore.UserId => tasks.UserTaskService[IO]) =
    TSecAuthService[User, TSecBearerToken[UserStore.UserId], IO] {
      case GET -> Root / "safe-resource" asAuthed user => Ok(s"Hello ${user.username}")
      case req @ POST -> Root / "add-task" asAuthed user =>
        for {
          desc <- req.request.as[String]
          _ <- userTaskService(user.id).add(desc)
        } yield Response[IO](Status.Ok)
      case GET -> Root / "list-task" asAuthed user =>
        for {
          ls <- userTaskService(user.id).list
        } yield Response[IO](Status.Ok).withEntity(ls.map(_.description))
    }

  def server(
      userCredentialStore: UserStore,
      tokenStore: BackingStore[IO, SecureRandomId, TSecBearerToken[UserStore.UserId]],
      taskService: tasks.TaskService[IO]): Kleisli[IO, Request[IO], Response[IO]] = {

    val authenticator = BearerTokenAuthenticator[IO, UserStore.UserId, User](
      tokenStore,
      userCredentialStore.identityStore,
      settings)
    val secureRoutes = SecuredRequestHandler(authenticator).liftService(privateRoutes(tasks.UserTaskService(_, taskService)))
    val loginRoutes = loginRoute(authenticator, userCredentialStore.checkPassword)

    (publicRoutes <+> loginRoutes <+> secureRoutes).orNotFound
  }

  def run(args: List[String]): IO[ExitCode] =
    for {
      userCredentialStore <- UserStore(
        UserStore.UsernamePasswordCredentials("username", "password")
      )
      tokenStore <- TokenStore.empty
      taskService <- tasks.TaskService.empty
      httpServer = server(userCredentialStore, tokenStore, taskService)
      loggingMiddleware = Logger.httpApp[IO](logHeaders = true, logBody = true)(_)
      res <- BlazeServerBuilder[IO]
        .bindHttp(port = 0)
        .withHttpApp(loggingMiddleware(httpServer))
        .serve
        .compile
        .drain
        .map(_ => ExitCode.Success)
        .recover { case _ => ExitCode.Error }
    } yield res
}
