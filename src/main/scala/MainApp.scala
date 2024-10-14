import agents.{Government, ManagerActor}
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.AskPattern._
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.headers.HttpOrigin
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._
import ch.megard.akka.http.cors.scaladsl.model.HttpHeaderRange
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import io.circe.syntax.EncoderOps
import middleware.JsonCodecs._
import spray.json.{JsArray, JsObject, JsString}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.io.StdIn


object MainApp {
  def main(args: Array[String]): Unit = {
    implicit val system: ActorSystem[ManagerActor.Command] = ActorSystem(ManagerActor(), "ManagerActorSystem")
    // Needed for the `ask` method below
    implicit val timeout: Timeout = Timeout(5.seconds) // Set your timeout duration here
    import system.executionContext // Needed for `Future` operations like `onSuccess`

    val settings = CorsSettings.defaultSettings
      .withAllowGenericHttpRequests(true) // To allow requests with any method
      .withAllowCredentials(true)
      .withAllowedOrigins { origin: HttpOrigin =>
        origin == HttpOrigin("http://localhost:3000")
      }
      .withAllowedHeaders(HttpHeaderRange.*)
      .withAllowedMethods(Seq(HttpMethods.GET, HttpMethods.POST, HttpMethods.PUT, HttpMethods.DELETE, HttpMethods.OPTIONS))
      .withExposedHeaders(Seq("X-Requested-With", "Authorization", "Content-Type", "Accept"))


    val gameRoutes: Route = {
      cors(settings) {
        concat(GovernmentRoutes.routes, RegionRoutes.routes)
      }
    }

    val bindingFuture = Http().newServerAt("localhost", 8080).bind(gameRoutes)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }
}
