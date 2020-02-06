package server

import cats.data.Kleisli
import cats.effect.IO
import org.http4s.circe.jsonOf
import org.http4s.implicits._ 
import org.http4s.util.CaseInsensitiveString
import org.http4s.{Method, Request, _}
import org.specs2._
import tsec.authentication
import tsec.authentication.TSecBearerToken
import tsec.common.SecureRandomId
import domain._
import login._

class AppServerSpec extends Specification {
  def is =
    s2"""
    App returns :
      200 for /non-safe-resource $openRouteCheck
      401 for /safe-resource $closedRouteCheck
      200 and `login token` for /login $loginRouteReturnsBearerToken
      200 for secure route after getting the token $secureRouteWithToken

      200 add task to task list of tasks $addTaskToList
      200 list tasks for specific user $userTaskList
  """

  implicit val listS: EntityDecoder[IO, List[String]] = jsonOf[IO, List[String]]

  private val newUser = UsernamePasswordCredentials("username", "password")

  private val appServerWithTaskService = for {
    userStore <- UserStore(newUser)
    tokenStore <- TokenStore.empty
    taskService <- tasks.TaskService.empty
  } yield (AppServer.routes(userStore, tokenStore, taskService), taskService, tokenStore)

  private val appServer = appServerWithTaskService.map(_._1)

  private def extractUserByToken(tokenStore: authentication.BackingStore[IO, SecureRandomId, TSecBearerToken[UserId]], token: Header) = {
    tokenStore.get(SecureRandomId(token.value.split(" ")(1))).map(_.identity).value
      .flatMap(uIdOpt => IO.fromEither(uIdOpt.toRight(new RuntimeException("Token Not Found"))))
  }

  def userLoginIntoServer(server: Kleisli[IO, Request[IO], Response[IO]]) = {
    val loginRequest = Request(method = Method.POST, uri = uri"/login").withEntity(newUser)

    for {
      response <- server.run(loginRequest)
        .map(_.headers.get(CaseInsensitiveString("Authorization")))
        .map(_.toRight(new RuntimeException("Can't login"): Throwable))
      token <- IO.fromEither(response)
    } yield token
  }

  def addTaskToList = (for {
    serverAndTaskS <- appServerWithTaskService
    (server, taskService, tokenStore) = serverAndTaskS
    token <- userLoginIntoServer(server)
    taskDescription = "task1"
    _ <- server.run(Request(method = Method.POST, uri = uri"/add-task").withHeaders(token).withEntity(taskDescription))
    userId <- extractUserByToken(tokenStore, token)
    ls <- taskService.list(userId)
  } yield ls.find(_.description == taskDescription).map(_.description) must beSome(taskDescription)).unsafeRunSync()


  def openRouteCheck = (for {
    server <- appServer
    res <- server.run(
      Request(method = Method.GET, uri = uri"/non-safe-resource") // public vs protected
    )
  } yield res.status must_=== Status.Ok).unsafeRunSync()

  def userTaskList = (for {
    server <- appServer
    token <- userLoginIntoServer(server)
    taskDescription = "task1"
    _ <- server.run(Request(method = Method.POST, uri = uri"/add-task").withHeaders(token).withEntity(taskDescription))
    response <- server.run(Request(method = Method.GET, uri = uri"/list-task").withHeaders(token).withEntity(taskDescription))
    res <- response.as[List[String]]
  } yield res must_=== List(taskDescription)).unsafeRunSync()

  def loginRouteReturnsBearerToken = {
    for {
      server <- appServer
      loginRequest = Request(method = Method.POST, uri = uri"/login").withEntity(newUser)
      response <- server.run(loginRequest)
    } yield (response.status must_=== Status.Ok) and
      (response.headers.get(CaseInsensitiveString("Authorization")).map(_.value) must beSome(beMatching("Bearer .*")))
    }.unsafeRunSync()


  def secureRouteWithToken = {

    for {
      server <- appServer
      token <- userLoginIntoServer(server)
      res <- server.run(Request(method = Method.GET, uri = uri"/safe-resource").withHeaders(token))
    } yield res.status must_=== Status.Ok
    }.unsafeRunSync()

  def closedRouteCheck = (for {
    server <- appServer
    res <- server.run(Request(method = Method.GET, uri = uri"/safe-resource"))
  } yield res.status must_=== Status.Unauthorized).unsafeRunSync()
}
