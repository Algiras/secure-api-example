package server.tasks

import org.http4s.Request

final case class UserServiceRequest[F[_]](request: Request[F], service: UserTaskService[F])
