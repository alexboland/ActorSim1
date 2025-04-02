import agents.{GameHistoryActor, ManagerActor}
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.actor.typed.scaladsl.AskPattern._
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.util.Timeout
import io.circe.syntax._
import io.circe.{Encoder, Json}
import middleware.{EventType, GameEvent}
import spray.json._

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.{Failure, Success}

object HistoryRoutes {
  // Custom JSON encoder for GameEvent
  implicit val eventTypeEncoder: Encoder[EventType] = Encoder.instance { eventType =>
    Json.obj("name" -> Json.fromString(eventType.name))
  }

  implicit val gameEventEncoder: Encoder[GameEvent] = Encoder.instance { event =>
    Json.obj(
      "timestamp" -> Json.fromLong(event.timestamp),
      "formattedTime" -> Json.fromString(event.formattedTime),
      "agentId" -> Json.fromString(event.agentId),
      "regionId" -> Json.fromString(event.regionId),
      "eventType" -> event.eventType.asJson,
      "eventText" -> Json.fromString(event.eventText)
    )
  }

  implicit val gameHistoryResponseEncoder: Encoder[GameHistoryActor.GameHistoryResponse] = Encoder.instance { response =>
    Json.obj(
      "events" -> Json.arr(response.events.map(_.asJson): _*),
      "totalCount" -> Json.fromInt(response.totalCount)
    )
  }

  def routes(implicit system: ActorSystem[ManagerActor.Command], timeout: Timeout = 5.seconds) = {
    // Get GameHistoryActor reference once for all routes
    implicit val ec = system.executionContext

    val gameHistoryActorFuture: Future[ActorRef[GameHistoryActor.Command]] =
      system.ask[ActorRef[GameHistoryActor.Command]](ManagerActor.GetGameHistoryActor)

    // Routes definition using the GameHistoryActor
    pathPrefix("history") {
      get {
        parameters(
          "agentId".as[String].optional,
          "regionId".as[String].optional,
          "eventType".as[String].optional,
          "fromTime".as[Long].optional,
          "toTime".as[Long].optional,
          "offset".as[Int].withDefault(0),
          "limit".as[Int].withDefault(100)
        ) { (agentId, regionId, eventTypeStr, fromTime, toTime, offset, limit) =>
          // Parse event type if provided
          val eventType = eventTypeStr.map {
            case "ResourceProduced" => EventType.ResourceProduced
            case "ResourceConsumed" => EventType.ResourceConsumed
            case "PopulationChanged" => EventType.PopulationChanged
            case "MarketTransaction" => EventType.MarketTransaction
            case "BondIssued" => EventType.BondIssued
            case "BondRepaid" => EventType.BondRepaid
            case "WorkerHired" => EventType.WorkerHired
            case "FounderCreated" => EventType.FounderCreated
            case "ConstructionStarted" => EventType.ConstructionStarted
            case "ConstructionCompleted" => EventType.ConstructionCompleted
            case "SeasonChanged" => EventType.SeasonChanged
            case custom => EventType.Custom(custom)
          }

          // First get the GameHistoryActor, then use it to get history
          val historyFuture = gameHistoryActorFuture.flatMap { historyActor =>
            historyActor.ask[GameHistoryActor.GameHistoryResponse](ref =>
              GameHistoryActor.GetGameHistory(
                agentId,
                regionId,
                eventType,
                fromTime,
                toTime,
                offset,
                limit,
                ref
              )
            )
          }

          onComplete(historyFuture) {
            case Success(response: GameHistoryActor.GameHistoryResponse) =>
              complete(HttpEntity(ContentTypes.`application/json`, response.asJson.noSpaces))
            case Failure(ex) =>
              complete(StatusCodes.InternalServerError ->
                JsObject("error" -> JsString(s"An error occurred: ${ex.getMessage}")).compactPrint)
          }
        }
      } ~
        path("recent") {
          get {
            parameter("limit".as[Int].withDefault(10)) { limit =>
              // Use the GameHistoryActor to get recent events
              val historyFuture = gameHistoryActorFuture.flatMap { historyActor =>
                historyActor.ask[GameHistoryActor.GameHistoryResponse](ref =>
                  GameHistoryActor.GetGameHistory(
                    None, None, None, None, None, 0, limit, ref
                  )
                )
              }

              onComplete(historyFuture) {
                case Success(response: GameHistoryActor.GameHistoryResponse) =>
                  complete(HttpEntity(ContentTypes.`application/json`, response.asJson.noSpaces))
                case Failure(ex) =>
                  complete(StatusCodes.InternalServerError ->
                    JsObject("error" -> JsString(s"An error occurred: ${ex.getMessage}")).compactPrint)
              }
            }
          }
        }
    }
  }
}