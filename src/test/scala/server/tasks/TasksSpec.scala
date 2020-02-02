package server.tasks

import cats.effect.IO
import org.specs2.Specification
import org.specs2.matcher.MatchResult
import server.UserStoreSpec

class TasksSpec extends Specification { def is =s2"""
      If you don't anything to task list it should be empty ${emptyCase.unsafeRunSync()}
      Adding task to a list returns it in list of tasks ${addTaskToList.unsafeRunSync()}
      Completing task makes it not accesible ${addTaskListAndRemove.unsafeRunSync()}
      Task can only be completed by it's creator ${taskOnlyDeletedByCreator.unsafeRunSync()}
      Only tasks created by the specific user can be visible for that user ${userVisibleTask.unsafeRunSync()}
    """

  private val emptyService: IO[TaskService[IO]] = TaskService.empty

  private val userIdGen = UserStoreSpec.randomUserId

  val emptyCase = for {
    service <- emptyService
    userId <- userIdGen
    ls <- service.list(userId)
  } yield ls must beEmpty

  val addTaskToList: IO[MatchResult[Option[TaskService.Task]]] = for {
    service <- emptyService
    userId <- userIdGen
    tsk <- service.add(userId, "Task")
    ls <- service.list(userId)
  } yield ls.find(_.id == tsk.id) must beSome(tsk)


  val addTaskListAndRemove = for {
    service <- emptyService
    userId <- userIdGen
    tsk <- service.add(userId, "Task")
    _ <- service.complete(userId, tsk.id)
    ls <- service.list(userId)
  } yield ls.find(_.id == tsk.id) must beNone

  val userVisibleTask = for {
    service <- emptyService
    userId <- userIdGen
    otherUserId <- userIdGen
    tsk1 <- service.add(userId, "Task1")
    tsk2 <- service.add(userId, "Task2")
    tsk3 <- service.add(otherUserId, "Task3")
    userList <- service.list(userId)
    otherUserList <- service.list(otherUserId)
  } yield (userList must contain(exactly(tsk1, tsk2))) and (otherUserList must contain(exactly(tsk3)))


  val taskOnlyDeletedByCreator = for {
    service <- emptyService
    creatorId <- UserStoreSpec.randomUserId
    completerId <- UserStoreSpec.randomUserId
    tsk <- service.add(creatorId, "Task")
    action <- service.complete(completerId, tsk.id).attempt
  } yield action must beLeft(TaskService.TaskCanOnlyBeCompletedByCreator: Throwable)
}
