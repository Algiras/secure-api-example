package server

import cats.Monad
import cats.effect.IO
import domain.UserId
import io.chrisdavenport.fuuid.FUUID
import org.http4s.circe.jsonEncoderOf
import org.http4s.dsl.io.{->, /, GET, POST, Root}
import org.http4s.{EntityEncoder, Request, Response, Status}
import server.UserStore.User
import shapeless.tag
import shapeless.tag.@@
import tsec.authentication.{SecuredRequest, TSecAuthService, TSecBearerToken}

package object tasks {
  type TaskId = FUUID @@ TaskIdTag

  def tagFUUIDAsTaskId(id: FUUID): TaskId = tag[TaskIdTag][FUUID](id)

  implicit val jsonStringListEncoder: EntityEncoder[IO, List[String]] = jsonEncoderOf[IO, List[String]]

  object withServices {
    def unapply[F[_]](req: UserServiceRequest[F]): Option[(Request[F], UserTaskService[F])] =
      Some(req.request -> req.service)
  }

  object UserService {
    def apply[F[_]: Monad](userTaskService: UserId => tasks.UserTaskService[F])(
                           from: PartialFunction[UserServiceRequest[F], F[Response[F]]]
  ): PartialFunction[SecuredRequest[F, User, TSecBearerToken[UserId]], F[Response[F]]] = from.compose[SecuredRequest[F, User, TSecBearerToken[UserId]]]{
      case req => UserServiceRequest[F](req.request, userTaskService(req.identity.id))
    }
  }

  def taskRoutes(userTaskService: UserId => tasks.UserTaskService[IO]): TSecAuthService[User, TSecBearerToken[UserId], IO] =
    TSecAuthService[User, TSecBearerToken[UserId], IO](UserService(userTaskService(_))({
      case req @ POST -> Root / "add-task" withServices uService =>
        for {
          desc <- req.request.as[String]
          _ <- uService.add(desc)
        } yield Response[IO](Status.Ok)
      case GET -> Root / "list-task" withServices uService =>
        for {
          ls <- uService.list
        } yield Response[IO](Status.Ok).withEntity(ls.map(_.description))
    }))
}
