package server

import cats.effect.IO
import cats.effect.concurrent.Ref
import io.chrisdavenport.fuuid.FUUID
import org.specs2._
import server.AppServer.UsernamePasswordCredentials
import server.UserStore.User
import org.http4s.{Method, Request, _}
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

  val username = "username"
  val password = "password"
  val newUser = UserStore.newUser(username, password)

  val appServer = for {
    user <- newUser
    userRef <- Ref.of[IO, Map[FUUID, User]](Map(user.id -> user))
    server <- AppServer.server(userRef)
  } yield server

  def openRouteCheck = (for {
    server <- appServer
    res <- server.run(
      Request(method = Method.GET, uri = uri"/non-safe-resource") // public vs protected
    )
  } yield res.status must_=== Status.Ok).unsafeRunSync()

  def loginRouteReturnsBearerToken = {
    for {
      server <- appServer
      loginRequest = Request(method = Method.POST, uri = uri"/login").withEntity(UsernamePasswordCredentials(username, password))
      response <- server.run(loginRequest)
    } yield (response.status must_=== Status.Ok) and
      (response.headers.get(CaseInsensitiveString("Authorization")).map(_.value) must beSome(beMatching("Bearer .*")))
    }.unsafeRunSync()


  def secureRouteWithToken = {

    for {
      server <- appServer
      loginRequest = Request(method = Method.POST, uri = uri"/login").withEntity(UsernamePasswordCredentials(username, password))
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
