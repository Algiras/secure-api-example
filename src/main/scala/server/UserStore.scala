package server

import java.util.UUID

import cats.data.OptionT
import cats.effect.IO
import cats.effect.concurrent.Ref
import tsec.authentication.IdentityStore

object UserStore {
  case class User(id: UUID, username: String, password: String)

  def apply(users: Ref[IO, Map[UUID, User]]): IdentityStore[IO, UUID, User] = new IdentityStore[IO, UUID, User] {
    override def get(id: UUID): OptionT[IO, User] = OptionT(users.get.map(_.get(id)))
  }
}
