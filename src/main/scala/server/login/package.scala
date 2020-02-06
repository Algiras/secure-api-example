package server

import cats.effect.IO
import domain.UserId
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes, Response, Status}
import org.http4s.circe.{jsonEncoderOf, jsonOf}
import org.http4s.dsl.io.{->, /, POST, Root}
import server.UserStore.User
import tsec.authentication.BearerTokenAuthenticator

package object login {
  implicit val loginUserEncoder: Encoder[UsernamePasswordCredentials] = deriveEncoder
  implicit val loginUserDecoder: Decoder[UsernamePasswordCredentials] = deriveDecoder

  implicit val entityLoginUserEncoder: EntityEncoder[IO, UsernamePasswordCredentials] =
    jsonEncoderOf[IO, UsernamePasswordCredentials]
  implicit val entityLoginUserDecoder: EntityDecoder[IO, UsernamePasswordCredentials] =
    jsonOf[IO, UsernamePasswordCredentials]

  def loginRoute(
                  auth: BearerTokenAuthenticator[IO, UserId, User],
                  checkPassword: UsernamePasswordCredentials => IO[Option[User]]): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case req @ POST -> Root / "login" =>
        (for {
          user <- req.as[UsernamePasswordCredentials]
          userOpt <- checkPassword(user)
        } yield userOpt).flatMap {
          case Some(user) => auth.create(user.id).map(auth.embed(Response(Status.Ok), _))
          case None => IO.pure(Response[IO](Status.Unauthorized))
        }
    }
}
