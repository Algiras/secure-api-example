package server.tasks

import cats.effect.IO
import org.specs2.Specification
import server.{UserStoreSpec, tasks}

class UserTaskSpec extends Specification { def is =
  s2"""
      Should work as TaskService, but with predefined user ${taskWithPreDefUser}
    """

  def taskWithPreDefUser = (for {
    userId <- UserStoreSpec.randomUserId
    taskService <- tasks.TaskService.empty
    uTaskService = tasks.UserTaskService(userId, taskService)
    tsk1 <- uTaskService.add("task1")
    tsk2 <- uTaskService.add("task2")
    tasksL <- uTaskService.list
    _ <- IO(tasksL must contain(tsk1, tsk2))
    _ <- uTaskService.complete(tsk1.id)
    tasks2L <- uTaskService.list
  } yield tasks2L must contain(tsk2) and not(contain(tsk1))).unsafeRunSync()

}
