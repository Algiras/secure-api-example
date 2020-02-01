package server

import java.time.Instant

import cats.effect.IO
import cats.effect.concurrent.Ref
import io.chrisdavenport.fuuid.FUUID
import org.specs2.Specification
import tsec.authentication.TSecBearerToken
import tsec.common.SecureRandomId

class TokenStoreSpec extends Specification { def is = s2"""
      Token Store:
        adding token let's you retrieve it ${setRetrieve.unsafeRunSync()}
        adding and then removing token makes the token unaccesible ${addRemove.unsafeRunSync()}
        updating token rewrites based on id ${update.unsafeRunSync()}
    """


  val buildRefStore = for {
    token <- FUUID.randomFUUID[IO].map(TSecBearerToken(SecureRandomId("token"), _, Instant.now(), None))
    tokens <- Ref.of[IO, Map[SecureRandomId, TSecBearerToken[FUUID]]](Map.empty)
    store = TokenStore(tokens)
    _ <- store.put(token)
  } yield (store, token)

  val setRetrieve = for {
    storeAndToken <- buildRefStore
    (store, token) = storeAndToken
    tokenRetrieved <- store.get(token.id).value
  } yield tokenRetrieved must beSome(token)

  val addRemove = for {
    storeAndToken <- buildRefStore
    (store, token) = storeAndToken
    _ <- store.delete(token.id)
    tokenRetrieved <- store.get(token.id).value
  } yield tokenRetrieved must beNone

  val update = for {
    storeAndToken <- buildRefStore
    (store, token) = storeAndToken
    token2 <- FUUID.randomFUUID[IO].map(TSecBearerToken(token.id, _, Instant.now(), None))
    _ <- store.update(token2)
    tokenRetrieved <- store.get(token.id).value
  } yield tokenRetrieved must beSome(token2)
}
