import agents.{Government, ManagerActor, ResourceType}
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.AskPattern._
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.util.Timeout
import io.circe.syntax.EncoderOps
import io.circe.parser._
import io.circe.{Decoder, Json}
import middleware.JsonCodecs._
import spray.json.{JsArray, JsObject, JsString, JsNumber}

import scala.concurrent.Future
import scala.concurrent.duration._

object GovernmentRoutes {
  // Case classes for request bodies
  case class SetPriceRequest(priceType: String, resourceType: String, price: Int)
  case class SetInterestRateRequest(rate: Double)

  // Add implicit decoders for the case classes
  implicit val setPriceRequestDecoder: Decoder[SetPriceRequest] =
    Decoder.forProduct3("priceType", "resourceType", "price")(SetPriceRequest.apply)

  implicit val setInterestRateRequestDecoder: Decoder[SetInterestRateRequest] =
    Decoder.forProduct1("rate")(SetInterestRateRequest.apply)

  def routes(implicit system: ActorSystem[ManagerActor.Command], timeout: Timeout) = pathPrefix("government") {
    concat(
      path("create") {
        post {
          // This route already takes a String, so we don't need to change it
          entity(as[String]) { jsonString =>
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
            case util.Success(maybeGovt) =>
              maybeGovt match {
                case Some(govt) =>
                  val jsonResponse = govt.asJson.noSpaces

                  // Create an HTTP entity with application/json content type
                  complete(HttpEntity(ContentTypes.`application/json`, jsonResponse))
                case None =>
                  complete(StatusCodes.NotFound -> JsObject("error" -> JsString("Region not found")).compactPrint)
              }
            case util.Failure(ex) =>
              println(s"error finding government: ${ex.getMessage}")
              complete(StatusCodes.InternalServerError -> s"An error occurred: ${ex.getMessage}")
          }
        }
      },
      path("setPrice") {
        post {
          // Changed to accept JSON instead of URL parameters
          entity(as[String]) { jsonString =>
            decode[SetPriceRequest](jsonString) match {
              case Right(request) =>
                val resourceTypeObj = resourceTypeFromString(request.resourceType)

                resourceTypeObj match {
                  case Some(resource) =>
                    val setFuture = system.ask(ref =>
                      ManagerActor.SetGovernmentPrice(request.priceType, resource, request.price, ref))

                    onComplete(setFuture) {
                      case util.Success(success: Boolean) =>
                        if (success) {
                          complete(HttpEntity(ContentTypes.`application/json`,
                            JsObject(
                              "status" -> JsString("success"),
                              "priceType" -> JsString(request.priceType),
                              "resourceType" -> JsString(request.resourceType),
                              "price" -> JsNumber(request.price)
                            ).compactPrint))
                        } else {
                          complete(StatusCodes.BadRequest ->
                            JsObject("error" -> JsString("Failed to set price")).compactPrint)
                        }
                      case util.Failure(ex) =>
                        complete(StatusCodes.InternalServerError ->
                          s"An error occurred: ${ex.getMessage}")
                    }
                  case None =>
                    complete(StatusCodes.BadRequest ->
                      JsObject("error" -> JsString(s"Invalid resource type: ${request.resourceType}")).compactPrint)
                }
              case Left(error) =>
                complete(StatusCodes.BadRequest ->
                  JsObject("error" -> JsString(s"Invalid JSON format: ${error.getMessage}")).compactPrint)
            }
          }
        }
      },
      path("setInterestRate") {
        post {
          // Changed to accept JSON instead of URL parameters
          entity(as[String]) { jsonString =>
            decode[SetInterestRateRequest](jsonString) match {
              case Right(request) =>
                if (request.rate < 0) {
                  complete(StatusCodes.BadRequest ->
                    JsObject("error" -> JsString("Interest rate cannot be negative")).compactPrint)
                } else {
                  val setFuture = system.ask(ref =>
                    ManagerActor.SetGovernmentInterestRate(request.rate, ref))

                  onComplete(setFuture) {
                    case util.Success(success: Boolean) =>
                      if (success) {
                        complete(HttpEntity(ContentTypes.`application/json`,
                          JsObject(
                            "status" -> JsString("success"),
                            "interestRate" -> JsNumber(request.rate)
                          ).compactPrint))
                      } else {
                        complete(StatusCodes.BadRequest ->
                          JsObject("error" -> JsString("Failed to set interest rate")).compactPrint)
                      }
                    case util.Failure(ex) =>
                      complete(StatusCodes.InternalServerError ->
                        s"An error occurred: ${ex.getMessage}")
                  }
                }
              case Left(error) =>
                complete(StatusCodes.BadRequest ->
                  JsObject("error" -> JsString(s"Invalid JSON format: ${error.getMessage}")).compactPrint)
            }
          }
        }
      }
    )
  }

  // Use the decoder from JsonCodecs instead of implementing our own function
  private def resourceTypeFromString(str: String): Option[ResourceType] = {
    import io.circe.parser._
    import middleware.JsonCodecs._

    decode[ResourceType](s"\"$str\"").toOption
  }
}