import agents.{ManagerActor, RegionActor}
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.util.Timeout
import io.circe.Json
import io.circe.syntax.EncoderOps
import middleware.JsonCodecs._
import spray.json.{JsObject, JsString}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}


object RegionRoutes {
  def routes(implicit system: ActorSystem[ManagerActor.Command], timeout: Timeout) = pathPrefix("regions") {
    concat(
      path("create") {
        post {
          entity(as[String]) { jsonString =>
            val createdActorFuture: Future[ManagerActor.RegionCreated] = system.ask(ManagerActor.CreateRandomRegion(_))
            onSuccess(createdActorFuture) { (created: ManagerActor.RegionCreated) =>
              /*val jsonResponse = JsObject(
                "uuid" -> JsString(uuidString),
                "population" -> JsNumber(region.population),
              )
              complete(HttpEntity(ContentTypes.`application/json`, jsonResponse.compactPrint))*/
              //println(created)
              created.actorRef match {
                case Right(actorRef) =>
                  complete(HttpEntity(ContentTypes.`application/json`, JsObject("uuid" -> JsString(s"${actorRef.path.name}")).compactPrint))
                case Left(error) =>
                  complete(HttpEntity(ContentTypes.`application/json`, JsObject("error" -> JsString(error)).compactPrint))
              }
            }
          }
        }
      },
      pathPrefix(Segment) { uuidString =>
        val uuid = uuidString
        concat(
          path("ping") {
            get {
              val infoFuture =
                system.ask(ref => ManagerActor.GetRegionInfo(uuid, ref))

              onComplete(infoFuture) {
                case util.Success(maybeRegion) =>
                  maybeRegion match {
                    case Some(RegionActor.InfoResponse(region)) =>
                      complete(StatusCodes.OK, HttpEntity(ContentTypes.`application/json`, region.asJson.noSpaces))
                    case None =>
                      complete(StatusCodes.NotFound -> JsObject("error" -> JsString("Region not found")).compactPrint)
                  }
                case util.Failure(ex) =>
                  complete(StatusCodes.InternalServerError -> s"An error occurred: ${ex.getMessage}")
                case _ =>
                  complete(StatusCodes.InternalServerError -> "Unexpected response type")
              }
            }
          },
          path("fullDetails") {
            get {
              val infoFuture = system.ask(ref => ManagerActor.GetFullRegionInfo(uuid, ref))

              onComplete(infoFuture) {
                case util.Success(maybeInfo) =>
                  maybeInfo match {
                    case Some(RegionActor.FullInfoResponse(region, agents)) => // TODO change this to give the actual full region info
                      val response = Json.obj(
                        "region" -> region.asJson,
                        "agents" -> agents.asJson)
                      complete(StatusCodes.OK, HttpEntity(ContentTypes.`application/json`, response.noSpaces))
                  }
              }
            }
          },
          pathPrefix("build" / Segment) { structure =>
            post {
              println(s"building ${structure} for region ${uuid}")
              val regionFuture: Future[Option[ActorRef[RegionActor.Command]]] = system.ask(ref => ManagerActor.GetRegionActor(uuid, ref))
                onComplete(regionFuture) {
                  case Success(response) =>
                    response match {
                      case Some(actorRef) =>
                        if (structure.equals("farm")) {
                          actorRef ! RegionActor.BuildFarm()
                        } else if (structure.equals("bank")) {
                          actorRef ! RegionActor.BuildBank()
                        }
                        complete(StatusCodes.OK)
                      case (None) =>
                        complete(StatusCodes.NotFound, s"couldn't find region ${uuid}")
                    }
                  case Failure(error) =>
                    println(error)
                    complete(error)
                  case _ =>
                    complete(StatusCodes.OK)
                }
            }
          }
        )
      })
  }
}