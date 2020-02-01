package server

import cats.effect.IO
import cats.effect.concurrent.Ref
import io.chrisdavenport.fuuid.FUUID
import server.TaskService.{Task, TaskId}
import server.UserStore.UserId
import shapeless.tag
import shapeless.tag.@@
import cats.syntax.monadError._

trait TaskService[F[_]] {
  def add(userId: UserId, taskDescription: String): F[Task]

  def complete(userId: UserId, taskId: TaskId): F[Unit]

  def list(userId: UserId): F[List[Task]]
}

object TaskService {

  trait TaskIdTag

  type TaskId = FUUID @@ TaskIdTag

  def tagFUUIDAsTaskId(id: FUUID): TaskId = tag[TaskIdTag][FUUID](id)

  case object TaskCanOnlyBeCompletedByCreator extends RuntimeException

  case class Task(id: TaskId, userId: UserId, description: String)

  val emptyTaskService: IO[TaskService[IO]] = for {
    tasks <- Ref.of[IO, Map[TaskId, Task]](Map.empty)
  } yield new TaskService[IO] {

    override def complete(userId: UserId, taskId: TaskId): IO[Unit] = tasks.modify(mp => {
      lazy val error = Left(TaskCanOnlyBeCompletedByCreator)
      lazy val success = Right(())

      mp.get(taskId).filter(_.userId == userId)
        .map(_ => {
          (mp - taskId, success)
        }).getOrElse((mp, error))
    }).rethrow

    override def add(userId: UserId, taskDescription: String): IO[Task] = FUUID.randomFUUID[IO]
      .map(tagFUUIDAsTaskId)
      .flatMap(id => tasks.modify(mp => {
        val task = Task(id, userId, taskDescription)
        (mp + (id -> task), task)
      }))

    override def list(userId: UserId): IO[List[Task]] = tasks.get.map(_.values.filter(_.userId == userId).toList)
  }
}
