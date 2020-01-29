package server

import java.time.Instant
import java.util.UUID

import cats.effect.IO
import cats.effect.concurrent.Ref
import org.specs2.Specification
import tsec.authentication.TSecBearerToken
import tsec.common.SecureRandomId

class TokenStoreSpec extends Specification { def is = s2"""
      Token Store:
        adding token let's you retrieve it ${setRetrieve.unsafeRunSync()}
        adding and then removing token makes the token unaccesible ${addRemove.unsafeRunSync()}
        updating token rewrites based on id ${update.unsafeRunSync()}
    """

  val token = TSecBearerToken(SecureRandomId("token"), UUID.randomUUID(), Instant.now(), None)
  val buildRefStore = for {
    tokens <- Ref.of[IO, Map[SecureRandomId, TSecBearerToken[UUID]]](Map.empty)
    store = TokenStore(tokens)
    _ <- store.put(token)
  } yield store

  val setRetrieve = for {
    store <- buildRefStore
    tokenRetrieved <- store.get(token.id).value
  } yield tokenRetrieved must beSome(token)

  val addRemove = for {
    store <- buildRefStore
    _ <- store.delete(token.id)
    tokenRetrieved <- store.get(token.id).value
  } yield tokenRetrieved must beNone

  val update = for {
    store <- buildRefStore
    token2 = TSecBearerToken(token.id, UUID.randomUUID(), Instant.now(), None)
    _ <- store.update(token2)
    tokenRetrieved <- store.get(token.id).value
  } yield tokenRetrieved must beSome(token2)
}
