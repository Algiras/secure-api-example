package server

import java.util.UUID

import cats.data.OptionT
import cats.effect.IO
import cats.effect.concurrent.Ref
import tsec.authentication.{BackingStore, TSecBearerToken}
import tsec.common.SecureRandomId

object TokenStore {
  def apply(ref: Ref[IO, Map[SecureRandomId, TSecBearerToken[UUID]]]): BackingStore[IO, SecureRandomId, TSecBearerToken[UUID]] = new BackingStore[IO, SecureRandomId, TSecBearerToken[UUID]] {
    override def put(elem: TSecBearerToken[UUID]): IO[TSecBearerToken[UUID]] =
      ref.modify(store => (store + (elem.id -> elem), elem))

    override def update(elem: TSecBearerToken[UUID]): IO[TSecBearerToken[UUID]] = put(elem)

    override def delete(id: SecureRandomId): IO[Unit] = ref.modify(store => (store - id, ()))

    override def get(id: SecureRandomId): OptionT[IO, TSecBearerToken[UUID]] = OptionT(ref.get.map(_.get(id)))
  }
}
