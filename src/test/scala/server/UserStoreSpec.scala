package server

import java.util.UUID

import cats.effect.IO
import cats.effect.concurrent.Ref
import server.UserStore.User
import org.specs2.Specification

class UserStoreSpec extends Specification { def is = s2"""
      User Store:
        retrieves existing users ${buildRefStore.unsafeRunSync()}
    """

   val user = User(UUID.randomUUID(), "username", "password")

  val buildRefStore = for {
    users <- Ref.of[IO, Map[UUID, User]](Map(user.id -> user))
    userStore = UserStore(users)
    retrievedUser <- userStore.get(user.id).value
  } yield retrievedUser must beSome(user)
}
