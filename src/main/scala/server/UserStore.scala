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
import cats.implicits._
import server.UserStore.{User, UserId, UsernamePasswordCredentials}

case class UserStore(
    identityStore: IdentityStore[IO, UserId, User],
    checkPassword: UsernamePasswordCredentials => IO[Option[User]]
)

object UserStore {
  type UserIdTag

  type UserId = FUUID @@ UserIdTag

  def tagFUUIDAsUserId(id: FUUID): UserId = tag[UserIdTag][FUUID](id)

  case class User(id: UserId, username: String, password: PasswordHash[BCrypt])

  def newUser(username: String, password: String): IO[User] =
    Applicative[IO].map2(
      FUUID.randomFUUID[IO].map(tagFUUIDAsUserId),
      BCrypt.hashpw[IO](password)
    )(User(_, username, _))

  case class UsernamePasswordCredentials(username: String, password: String)

  private def validateUser(credentials: UsernamePasswordCredentials)(users: List[User]): IO[Option[User]] =
    users.findM(
      user =>
        BCrypt
          .checkpwBool[IO](credentials.password, user.password)
          .map(_ && credentials.username == user.username),
    )

  def apply(user: UsernamePasswordCredentials, users: UsernamePasswordCredentials*): IO[UserStore] =
    for {
      userList <- (user +: users)
        .map(u => UserStore.newUser(u.username, u.password))
        .toList
        .sequence
      users <- Ref.of[IO, Map[UserStore.UserId, User]](userList.map(u => u.id -> u).toMap)
    } yield
      new UserStore(
        (id: UserId) => OptionT(users.get.map(_.get(id))),
        usr => users.get.map(_.values.toList).flatMap(validateUser(usr)(_))
      )

}
