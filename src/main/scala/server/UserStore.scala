package server

import cats.Applicative
import cats.data.OptionT
import cats.effect.IO
import cats.effect.concurrent.Ref
import io.chrisdavenport.fuuid.FUUID
import shapeless.tag
import shapeless.tag.@@
import tsec.authentication.IdentityStore
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.BCrypt

object UserStore {
  type UserIdTag

  type UserId = FUUID @@ UserIdTag

  def tagFUUIDAsUserId(id: FUUID): UserId = tag[UserIdTag][FUUID](id)

  case class User(id: UserId, username: String, password: PasswordHash[BCrypt])

  def newUser(username: String, password: String): IO[User] = Applicative[IO].map2(
    FUUID.randomFUUID[IO].map(tagFUUIDAsUserId),
    BCrypt.hashpw[IO](password)
  )(User(_, username, _))

  def apply(users: Ref[IO, Map[FUUID, User]]): IdentityStore[IO, FUUID, User] =
    (id: FUUID) => OptionT(users.get.map(_.get(id)))
}
