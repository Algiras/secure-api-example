package server

import cats.data.OptionT
import cats.effect.IO
import cats.effect.concurrent.Ref
import server.UserStore.UserId
import tsec.authentication.{BackingStore, TSecBearerToken}
import tsec.common.SecureRandomId

object TokenStore {
  def apply(ref: Ref[IO, Map[SecureRandomId, TSecBearerToken[UserId]]]): BackingStore[
    IO,
    SecureRandomId,
    TSecBearerToken[UserStore.UserId]] =
    new BackingStore[IO, SecureRandomId, TSecBearerToken[UserId]] {
      override def put(elem: TSecBearerToken[UserId]): IO[TSecBearerToken[UserId]] =
        ref.modify(store => (store + (elem.id -> elem), elem))

      override def update(elem: TSecBearerToken[UserId]): IO[TSecBearerToken[UserId]] = put(elem)

      override def delete(id: SecureRandomId): IO[Unit] = ref.modify(store => (store - id, ()))

      override def get(id: SecureRandomId): OptionT[IO, TSecBearerToken[UserId]] =
        OptionT(ref.get.map(_.get(id)))
    }

  val empty: IO[BackingStore[IO, SecureRandomId, TSecBearerToken[UserId]]] = for {
    tokens <- Ref.of[IO, Map[SecureRandomId, TSecBearerToken[UserStore.UserId]]](Map.empty)
  } yield TokenStore(tokens)
}
