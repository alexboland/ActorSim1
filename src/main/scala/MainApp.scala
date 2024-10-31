import agents.{Government, ManagerActor}
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.AskPattern.*
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.headers.{HttpOrigin, HttpOriginRange, `Access-Control-Allow-Credentials`, `Access-Control-Allow-Headers`, `Access-Control-Allow-Methods`, `Access-Control-Allow-Origin`, `Access-Control-Expose-Headers`}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import io.circe.syntax.EncoderOps
import middleware.JsonCodecs.*
import spray.json.{JsArray, JsObject, JsString}

import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.io.StdIn


object MainApp {
  def main(args: Array[String]): Unit = {
    implicit val system: ActorSystem[ManagerActor.Command] = ActorSystem(ManagerActor(), "ManagerActorSystem")
    // Needed for the `ask` method below
    implicit val timeout: Timeout = Timeout(5.seconds) // Set your timeout duration here
    import system.executionContext // Needed for `Future` operations like `onSuccess`

    object CorsSupport:
      private val corsHeaders = List(
        `Access-Control-Allow-Origin`.*,
        `Access-Control-Allow-Credentials`(true),
        `Access-Control-Allow-Methods`(
          HttpMethods.GET,
          HttpMethods.POST,
          HttpMethods.PUT,
          HttpMethods.DELETE,
          HttpMethods.OPTIONS
        ),
        `Access-Control-Allow-Headers`(
          "Authorization",
          "Content-Type",
          "X-Requested-With",
          "Accept"
        ),
        `Access-Control-Expose-Headers`(
          "X-Requested-With",
          "Authorization",
          "Content-Type",
          "Accept"
        )
      )

      def withCors(routes: Route): Route =
        respondWithHeaders(corsHeaders) {
          routes
        } ~ options {
          // For preflight requests, respond with allowed methods and headers
          complete(HttpResponse(StatusCodes.OK).withHeaders(corsHeaders))
        }

    object GameRoutes:
      val routes: Route = CorsSupport.withCors {
        concat(
          GovernmentRoutes.routes,
          RegionRoutes.routes
        )
      }

    val bindingFuture = Http().newServerAt("localhost", 8080).bind(GameRoutes.routes)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }
}
