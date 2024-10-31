import agents.{Government, ManagerActor}
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.AskPattern._
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.util.Timeout
import io.circe.syntax.EncoderOps
import middleware.JsonCodecs._
import spray.json.{JsArray, JsObject, JsString}

import scala.concurrent.Future
import scala.concurrent.duration._

object GovernmentRoutes {
  def routes(implicit system: ActorSystem[ManagerActor.Command], timeout: Timeout) = pathPrefix("government") {
    concat(
      path("create") {
        post {
          entity(as[String]) { jsonString => //keeping this in here in case we need it later
            val createdGovtFuture = system.ask(ManagerActor.CreateGovernment(_))
            onSuccess(createdGovtFuture) { created =>
              created match {
                case Some(govt: Government) =>
                  complete {
                    // Extract keys and convert to List[String]
                    val keysList: List[String] = govt.regions.keys.toList

                    // Convert each String in the list to a JsString and create a JsArray
                    val jsArray = JsArray(keysList.map(JsString(_)): _*)

                    // Construct a JsObject with the key "regions" mapped to the jsArray
                    val jsonResponse = JsObject("regions" -> jsArray)

                    // Create an HTTP entity with application/json content type and the compact JSON print of the JsObject
                    HttpEntity(ContentTypes.`application/json`, jsonResponse.compactPrint)
                  }
                case None =>
                  complete {
                    val jsonResponse = JsObject("error" -> JsString("something went wrong"))
                    HttpEntity(ContentTypes.`application/json`, jsonResponse.compactPrint)
                  }
              }
            }
          }
        }
      },
      path("ping") {
        get {
          implicit val timeout: Timeout = 3.seconds
          val infoFuture: Future[Option[Government]] =
            system.ask(ref => ManagerActor.GetGovtInfo(ref))

          onComplete(infoFuture) {
            case util.Success(maybeRegion) =>
              maybeRegion match {
                case Some(govt) =>
                  val jsonResponse = govt.asJson.noSpaces //JsObject("regions" -> jsArray)

                  // Create an HTTP entity with application/json content type and the compact JSON print of the JsObject
                  HttpEntity(ContentTypes.`application/json`, jsonResponse)
                  complete(HttpEntity(ContentTypes.`application/json`, jsonResponse))
                case None =>
                  complete(StatusCodes.NotFound -> JsObject("error" -> JsString("Region not found")).compactPrint)
              }
            case util.Failure(ex) =>
              println(s"error finding government: ${ex.getMessage}")
              complete(StatusCodes.InternalServerError -> s"An error occurred: ${ex.getMessage}")
          }
        }
      }
    )
  }
}