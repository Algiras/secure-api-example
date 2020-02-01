package server

import cats.data.OptionT
import cats.effect.IO
import cats.effect.concurrent.Ref
import io.chrisdavenport.fuuid.FUUID
import tsec.authentication.{BackingStore, TSecBearerToken}
import tsec.common.SecureRandomId

object TokenStore {
  def apply(ref: Ref[IO, Map[SecureRandomId, TSecBearerToken[FUUID]]]): BackingStore[IO, SecureRandomId, TSecBearerToken[FUUID]] = new BackingStore[IO, SecureRandomId, TSecBearerToken[FUUID]] {
    override def put(elem: TSecBearerToken[FUUID]): IO[TSecBearerToken[FUUID]] =
      ref.modify(store => (store + (elem.id -> elem), elem))

    override def update(elem: TSecBearerToken[FUUID]): IO[TSecBearerToken[FUUID]] = put(elem)

    override def delete(id: SecureRandomId): IO[Unit] = ref.modify(store => (store - id, ()))

    override def get(id: SecureRandomId): OptionT[IO, TSecBearerToken[FUUID]] = OptionT(ref.get.map(_.get(id)))
  }
}
