package server

import cats.Applicative
import cats.data.OptionT
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._
import domain._
import io.chrisdavenport.fuuid.FUUID
import server.UserStore.User
import server.login.UsernamePasswordCredentials
import tsec.authentication.IdentityStore
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.BCrypt

case class UserStore(
    identityStore: IdentityStore[IO, UserId, User],
    checkPassword: UsernamePasswordCredentials => IO[Option[User]]
)

object UserStore {
  case class User(id: UserId, username: String, password: PasswordHash[BCrypt])

  def newUser(username: String, password: String): IO[User] =
    Applicative[IO].map2(
      FUUID.randomFUUID[IO].map(tagFUUIDAsUserId),
      BCrypt.hashpw[IO](password)
    )(User(_, username, _))

  private def validateUser(credentials: UsernamePasswordCredentials)(
      users: List[User]): IO[Option[User]] =
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
      users <- Ref.of[IO, Map[UserId, User]](userList.map(u => u.id -> u).toMap)
    } yield
      new UserStore(
        (id: UserId) => OptionT(users.get.map(_.get(id))),
        usr => users.get.map(_.values.toList).flatMap(validateUser(usr)(_))
      )

}
