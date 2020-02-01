package server

import cats.effect.IO
import cats.effect.concurrent.Ref
import io.chrisdavenport.fuuid.FUUID
import org.specs2.Specification
import server.UserStore.{User, UserId}

class UserStoreSpec extends Specification { def is = s2"""
      User Store:
        retrieves existing users ${buildRefStore.unsafeRunSync()}
    """

  val buildRefStore = for {
    user <- UserStore.newUser("username", "password")
    users <- Ref.of[IO, Map[FUUID, User]](Map(user.id -> user))
    userStore = UserStore(users)
    retrievedUser <- userStore.get(user.id).value
  } yield retrievedUser must beSome(user)
}

object UserStoreSpec {
  def randomUserId: IO[UserId] = FUUID.randomFUUID[IO].map(UserStore.tagFUUIDAsUserId)
}
