package server

import _root_.server.UserStore.User
import cats.Applicative
import cats.data.Kleisli
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import io.chrisdavenport.fuuid.FUUID
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import org.http4s._
import org.http4s.circe.{jsonEncoderOf, jsonOf}
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import tsec.authentication.{BearerTokenAuthenticator, SecuredRequestHandler, TSecAuthService, _}
import tsec.common.SecureRandomId
import tsec.passwordhashers.jca.BCrypt

import scala.concurrent.duration._

object AppServer extends IOApp {

  implicit val FUUIDEncoder: Encoder[FUUID] = Encoder.instance(_.show.asJson)
  implicit val FUUIDDecoder: Decoder[FUUID] = Decoder[String].emap(FUUID.fromString(_).leftMap(_.toString))

  case class UsernamePasswordCredentials(username: String, password: String)

  implicit val loginUserEncoder: Encoder[UsernamePasswordCredentials] = deriveEncoder
  implicit val loginUserDecoder: Decoder[UsernamePasswordCredentials] = deriveDecoder

  implicit val entityLoginUserEncoder: EntityEncoder[IO, UsernamePasswordCredentials] = jsonEncoderOf[IO, UsernamePasswordCredentials]
  implicit val entityLoginUserDecoder: EntityDecoder[IO, UsernamePasswordCredentials] = jsonOf[IO, UsernamePasswordCredentials]

  val settings: TSecTokenSettings = TSecTokenSettings(
    expiryDuration = 10.minute,
    maxIdle = None
  )

  def buildAuthenticator(users: Ref[IO, Map[FUUID, User]]): IO[BearerTokenAuthenticator[IO, FUUID, User]] = for {
    tokens <- Ref.of[IO, Map[SecureRandomId, TSecBearerToken[FUUID]]](Map.empty)
    tokenStore = TokenStore(tokens)
    userStore = UserStore(users)
  } yield BearerTokenAuthenticator[IO, FUUID, User](tokenStore, userStore, settings)

  def loginRoute(auth: BearerTokenAuthenticator[IO, FUUID, User],
                 checkPassword: UsernamePasswordCredentials => IO[Option[User]]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req@POST -> Root / "login" => (for {
      user <- req.as[UsernamePasswordCredentials]
      userOpt <- checkPassword(user)
    } yield userOpt).flatMap {
      case Some(user) => auth.create(user.id).map(auth.embed(Response(Status.Ok), _))
      case None => IO.pure(Response[IO](Status.Unauthorized))
    }
  }

  val publicRoutes = HttpRoutes.of[IO] {
    case GET -> Root / "non-safe-resource" => Ok()
  }

  val privateRoutes = TSecAuthService[User, TSecBearerToken[FUUID], IO] {
    case GET -> Root / "safe-resource" asAuthed user => Ok(s"Hello ${user.username}")
  }

  // case where you don't need user info...

  def server(users: Ref[IO, Map[FUUID, User]]): IO[Kleisli[IO, Request[IO], Response[IO]]] = for {
    authenticator <- buildAuthenticator(users)
    secureRoutes = SecuredRequestHandler(authenticator).liftService(privateRoutes)
    loginRoutes = loginRoute(authenticator, userCredentials => users.get.flatMap(mp =>
      mp.values.toList.findM(usr =>
        Applicative[IO].map2(
          BCrypt.checkpwBool[IO](userCredentials.password, usr.password),
          IO.pure(userCredentials.username == usr.username)
        )(_ && _)
    )))
  } yield (publicRoutes <+> loginRoutes <+> secureRoutes).orNotFound

  def run(args: List[String]): IO[ExitCode] = for {
    user <- UserStore.newUser("username", "password")
    users <- Ref.of[IO, Map[FUUID, User]](Map(user.id -> user))
    httpServer <- server(users)
    loggingMiddleware = Logger.httpApp[IO](logHeaders = true, logBody = true)(_)
    _ <- BlazeServerBuilder[IO].bindHttp(port = 0)
      .withHttpApp(loggingMiddleware(httpServer))
      .serve.compile.drain
  } yield ExitCode.Success
}
