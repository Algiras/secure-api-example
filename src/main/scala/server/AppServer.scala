package server

import java.util.UUID

import cats.data.Kleisli
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import io.circe._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.http4s._
import org.http4s.circe.{jsonEncoderOf, jsonOf}
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import _root_.server.UserStore.User
import tsec.authentication.{BearerTokenAuthenticator, SecuredRequestHandler, TSecAuthService, _}
import tsec.common.SecureRandomId

import scala.concurrent.duration._

object AppServer extends IOApp {

  case class LoginUser(username: String, password: String)

  implicit val loginUserEncoder: Encoder[LoginUser] = deriveEncoder
  implicit val loginUserDecoder: Decoder[LoginUser] = deriveDecoder

  implicit val entityLoginUserEncoder: EntityEncoder[IO, LoginUser] = jsonEncoderOf[IO, LoginUser]
  implicit val entityLoginUserDecoder: EntityDecoder[IO, LoginUser] = jsonOf[IO, LoginUser]

  val settings: TSecTokenSettings = TSecTokenSettings(
    expiryDuration = 10.minute,
    maxIdle = None
  )

  def buildAuthenticator(users: Ref[IO, Map[UUID, User]]): IO[BearerTokenAuthenticator[IO, UUID, User]] = for {
    tokens <- Ref.of[IO, Map[SecureRandomId, TSecBearerToken[UUID]]](Map.empty)
    tokenStore = TokenStore(tokens)
    userStore = UserStore(users)
  } yield BearerTokenAuthenticator[IO, UUID, User](tokenStore, userStore, settings)

  def loginRoute(auth: BearerTokenAuthenticator[IO, UUID, User],
                 findUser: LoginUser => IO[Option[User]]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req@POST -> Root / "login" => (for {
      user <- req.as[LoginUser]
      userOpt <- findUser(user)
    } yield userOpt).flatMap {
      case Some(user) => auth.create(user.id).map(auth.embed(Response(Status.Ok), _))
      case None => IO.pure(Response[IO](Status.Unauthorized))
    }
  }

  val publicRoutes = HttpRoutes.of[IO] {
    case GET -> Root / "non-safe-resource" => Ok()
  }

  val privateRoutes = TSecAuthService[User, TSecBearerToken[UUID], IO] {
    case GET -> Root / "safe-resource" asAuthed user => Ok(s"Hello ${user.username}")
  }

  // case where you don't need user info...

  def server(users: Ref[IO, Map[UUID, User]]): IO[Kleisli[IO, Request[IO], Response[IO]]] = for {
    authenticator <- buildAuthenticator(users)
    secureRoutes = SecuredRequestHandler(authenticator).liftService(privateRoutes)
    loginRoutes = loginRoute(authenticator, loginUser => users.get.map(_.values.find(usr =>
        loginUser.username == usr.username && loginUser.password == usr.password
      ))
    )
  } yield (publicRoutes <+> loginRoutes <+> secureRoutes).orNotFound

  val user = User(UUID.randomUUID(), "username", "password")

  def run(args: List[String]): IO[ExitCode] = for {
    users <- Ref.of[IO, Map[UUID, User]](Map(user.id -> user))
    httpServer <- server(users)
    loggingMiddleware = Logger.httpApp[IO](logHeaders = true, logBody = true)(_)
    _ <- BlazeServerBuilder[IO].bindHttp(port = 0)
      .withHttpApp(loggingMiddleware(httpServer))
      .serve.compile.drain
  } yield ExitCode.Success
}
