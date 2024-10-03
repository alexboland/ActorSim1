import JsonCodecs._
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
        concat(
        pathPrefix("government") {
          concat(
            path("create") {
              post {
                entity(as[String]) { jsonString => //keeping this in here in case we need it later
                  val createdGovtFuture = system.ask(ManagerActor.CreateGovernment(_))
                  onSuccess(createdGovtFuture) { created =>
                    created match {
                      case Some(govt) =>
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
                        println(s"found govt")
                        // Extract keys and convert to List[String]
                        val keysList: List[String] = govt.regions.keys.toList

                        // Convert each String in the list to a JsString and create a JsArray
                        val jsArray = JsArray(keysList.map(JsString(_)): _*)

                        // Construct a JsObject with the key "regions" mapped to the jsArray
                        val jsonResponse = govt.asJson.noSpaces //JsObject("regions" -> jsArray)

                        // Create an HTTP entity with application/json content type and the compact JSON print of the JsObject
                        HttpEntity(ContentTypes.`application/json`, jsonResponse)
                        complete(HttpEntity(ContentTypes.`application/json`, jsonResponse))
                      case None =>
                        println(s"no government found")
                        complete(StatusCodes.NotFound -> JsObject("error" -> JsString("Region not found")).compactPrint)
                    }
                  case util.Failure(ex) =>
                    println(s"error finding government: ${ex.getMessage}")
                    complete(StatusCodes.InternalServerError -> s"An error occurred: ${ex.getMessage}")
                  case _ =>
                    println(s"Unexpected response type for government")
                    complete(StatusCodes.InternalServerError -> "Unexpected response type")
                }
              }
            }
          )
        },
        pathPrefix("regions") {
          concat(
            path("create") {
              post {
                entity(as[String]) { jsonString =>
                  println("=====BEGINNING REGIONS ROUTE=======")
                  val createdActorFuture = system.ask(ManagerActor.CreateRandomRegion(_))
                  onSuccess(createdActorFuture) { created =>
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
            path("ping" / Segment) { uuidString =>
              get {
                val uuid = uuidString
                println(s"pinging for ${uuid}")
                implicit val timeout: Timeout = 3.seconds
                val infoFuture: Future[Option[Region]] =
                  system.ask(ref => ManagerActor.GetRegionInfo(uuid, ref))

                onComplete(infoFuture) {
                  case util.Success(maybeRegion) =>
                      maybeRegion match {
                        case Some(region) =>
                          println(s"found region ${uuidString}")
                          complete(StatusCodes.OK, HttpEntity(ContentTypes.`application/json`, region.asJson.noSpaces))
                        case None =>
                          println(s"no region found for ${uuidString}")
                          complete(StatusCodes.NotFound -> JsObject("error" -> JsString("Region not found")).compactPrint)
                    }
                  case util.Failure(ex) =>
                    println(s"error finding region ${uuidString}: ${ex.getMessage}")
                    complete(StatusCodes.InternalServerError -> s"An error occurred: ${ex.getMessage}")
                  case _ =>
                    println(s"Unexpected response type for ${uuidString}")
                    complete(StatusCodes.InternalServerError -> "Unexpected response type")
                }
            }})
        })
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
