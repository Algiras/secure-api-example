package server

import java.util.UUID

import cats.effect.IO
import cats.effect.concurrent.Ref
import server.AppServer.LoginUser
import server.UserStore.User
import org.specs2._
import org.http4s.Request
import org.http4s.Method
import org.http4s._
import org.http4s.implicits._
import org.http4s.util.CaseInsensitiveString

class AppServerSpec extends Specification {
  def is =
    s2"""
    App returns :
      200 for /non-safe-resource $openRouteCheck
      401 for /safe-resource $closedRouteCheck
      200 and `login token` for /login $loginRouteReturnsBearerToken
      200 for secure route after getting the token $secureRouteWithToken
  """

  val user = User(UUID.randomUUID(), "username", "password")
  val appServer = for {
    userRef <- Ref.of[IO, Map[UUID, User]](Map(user.id -> user))
    server <- AppServer.server(userRef)
  } yield server

  def openRouteCheck = (for {
    server <- appServer
    res <- server.run(
      Request(method = Method.GET, uri = uri"/non-safe-resource") // public vs protected
    )
  } yield res.status must_=== Status.Ok).unsafeRunSync()

  def loginRouteReturnsBearerToken = {
    val user = User(UUID.randomUUID(), "username", "password")

    for {
      server <- appServer
      loginRequest = Request(method = Method.POST, uri = uri"/login").withEntity(LoginUser(user.username, user.password))
      response <- server.run(loginRequest)
    } yield (response.status must_=== Status.Ok) and
      (response.headers.get(CaseInsensitiveString("Authorization")).map(_.value) must beSome(beMatching("Bearer .*")))
    }.unsafeRunSync()


  def secureRouteWithToken = {
    val user = User(UUID.randomUUID(), "username", "password")

    for {
      server <- appServer
      loginRequest = Request(method = Method.POST, uri = uri"/login").withEntity(LoginUser(user.username, user.password))
      response <- server.run(loginRequest)
        .map(_.headers.get(CaseInsensitiveString("Authorization")))
        .map(_.toRight(new RuntimeException("Can't login"): Throwable))
      token <- IO.fromEither(response)
      res <- server.run(Request(method = Method.GET, uri = uri"/safe-resource").withHeaders(token))
    } yield res.status must_=== Status.Ok
    }.unsafeRunSync()

  def closedRouteCheck = (for {
    server <- appServer
    res <- server.run(Request(method = Method.GET, uri = uri"/safe-resource"))
  } yield res.status must_=== Status.Unauthorized).unsafeRunSync()
}
