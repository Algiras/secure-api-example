package server.tasks

import server.UserStore.UserId
import server.tasks.TaskService.{Task, TaskId}

trait UserTaskService[F[_]] {
  def add(taskDescription: String): F[Task]

  def complete(taskId: TaskId): F[Unit]

  def list: F[List[Task]]
}

object UserTaskService {
  def apply[F[_]](userId: UserId, taskService: TaskService[F]): UserTaskService[F] = new UserTaskService[F] {
    override def add(taskDescription: String): F[Task] = taskService.add(userId, taskDescription)

    override def complete(taskId: TaskId): F[Unit] = taskService.complete(userId, taskId)

    override val list: F[List[Task]] = taskService.list(userId)
  }
}
