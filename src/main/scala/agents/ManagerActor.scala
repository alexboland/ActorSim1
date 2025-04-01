package agents

import agents.RegionActor.ShowFullInfo
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout
import middleware.{EventType, GameEvent, GameHistory, GameInfo}

import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, DurationInt}
import scala.util.{Failure, Success}
import akka.actor.typed.scaladsl.AskPattern.Askable

import java.time.Instant

object ManagerActor {
  trait Command
  case class CreateGovernment(replyTo: ActorRef[Option[Government]]) extends Command
  case class CreateRegions(locations: List[Region.Location], replyTo: ActorRef[RegionsCreated]) extends Command
  case class CreateRandomRegion(x: Int, y: Int, replyTo: ActorRef[RegionCreated]) extends Command
  case class RegionCreated(actorRef: Either[String, ActorRef[RegionActor.Command]]) extends Command
  case class RegionsCreated(resp: Either[String, List[ActorRef[RegionActor.Command]]]) extends Command
  case class GetGovtInfo(replyTo: ActorRef[Option[Government]]) extends Command
  case class GetRegionInfo(uuid: String, replyTo: ActorRef[Option[GameInfo.InfoResponse]]) extends Command
  case class GetRegionsInfo(replyTo: ActorRef[List[RegionActor.InfoResponse]]) extends Command
  case class AggregateRegionsInfo(replyTo: ActorRef[List[RegionActor.InfoResponse]], requestId: UUID, response: Option[RegionActor.InfoResponse], expectedSize: Int) extends Command
  case class GetFullRegionInfo(uuid: String, replyTo: ActorRef[Option[GameInfo.InfoResponse]]) extends Command
  case class GetRegionActor(uuid: String, replyTo: ActorRef[Option[ActorRef[RegionActor.Command]]]) extends Command
  case class SetGovernmentPrice(priceType: String, resourceType: ResourceType, price: Int, replyTo: ActorRef[Boolean]) extends Command
  case class SetGovernmentInterestRate(rate: Double, replyTo: ActorRef[Boolean]) extends Command
  case class ActorNoOp() extends Command


  // New commands for game history
  case class AddGameEvent(event: GameEvent) extends Command
  case class GetGameHistory(
                             agentId: Option[String] = None,
                             regionId: Option[String] = None,
                             eventType: Option[EventType] = None,
                             fromTime: Option[Long] = None,
                             toTime: Option[Long] = None,
                             offset: Int = 0,
                             limit: Int = 100,
                             replyTo: ActorRef[GameHistoryResponse]
                           ) extends Command

  case class GameHistoryResponse(events: Vector[GameEvent], totalCount: Int)

  // Helper method to create events easily
  def createEvent(
                   agentId: String,
                   regionId: String,
                   eventType: EventType,
                   eventText: String
                 ): GameEvent = {
    GameEvent(
      timestamp = System.currentTimeMillis(),
      agentId = agentId,
      regionId = regionId,
      eventType = eventType,
      eventText = eventText
    )
  }

  private case class InternalRegionResponse(regionOpt: Option[Region]) extends Command
  private case class InternalGovtResponse(govtOpt: Option[Government], replyTo: ActorRef[Option[Government]]) extends Command

  private var government: Option[ActorRef[GovernmentActor.Command]] = None
  private var regions = Map.empty[String, ActorRef[RegionActor.Command]]
  private var inProgressCollections = Map.empty[UUID, List[RegionActor.InfoResponse]]

  // Game history storage
  private var gameHistory = GameHistory()

  implicit val timeout: Timeout = 3.seconds

  def apply(): Behavior[Command] = Behaviors.setup { context =>
    implicit val ec = context.executionContext
    implicit val scheduler = context.system.scheduler
    Behaviors.receiveMessage {
      case CreateGovernment(replyTo) =>
        println("====CREATE GOVT COMMAND=====")
        government match {
          case Some(actorRef) =>
            context.ask(actorRef, ShowInfo.apply) {
              case Success(Some(GovernmentActor.InfoResponse(govt))) =>
                println("success with govt ping!")
                InternalGovtResponse(Some(govt), replyTo)
              case Failure(f) =>
                println(s"failure due to ${f.toString}")
                InternalGovtResponse(None, replyTo)
            }

          case None =>
            println("====NO GOVT FOUND=====")
            val actorRef = context.spawn(GovernmentActor(), "Government")
            government = Some(actorRef)
            actorRef ! GovernmentActor.InitializeGov(Government.newGov())

            // Log event to game history
            val event = createEvent(
              "system",
              "global",
              EventType.Custom("GovernmentCreated"),
              "A new government was established"
            )
            context.self ! AddGameEvent(event)

            context.ask(actorRef, ShowInfo.apply) {
              case Success(Some(GovernmentActor.InfoResponse(govt))) =>
                println("success with govt ping!")
                InternalGovtResponse(Some(govt), replyTo)
              case Failure(f) =>
                println(s"failure due to ${f.toString}")
                InternalGovtResponse(None, replyTo)
            }
        }
        Behaviors.same

      case GetGovtInfo(replyTo) =>
        government match {
          case Some(actorRef) =>
            context.ask(actorRef, ShowInfo.apply) {
              case Success(Some(GovernmentActor.InfoResponse(govt))) =>
                println("success with govt ping!")
                InternalGovtResponse(Some(govt), replyTo)
              case Failure(f) =>
                println(s"failure due to ${f.toString}")
                InternalGovtResponse(None, replyTo)
            }
          case None =>
            InternalGovtResponse(None, replyTo)
        }
        Behaviors.same

      case InternalGovtResponse(govtOpt, originalReplyTo) =>
        originalReplyTo ! govtOpt
        Behaviors.same

      case CreateRegions(locations, replyTo) =>
        println("======CREATE REGIONS CALLED========")
        government match {
          case Some(govt) =>
            val actorRefs = locations.foldLeft(List[ActorRef[RegionActor.Command]]()) { (acc, location) =>
              val region = Region.newRandomRegion(location.x, location.y)
              val state = RegionActorState(region = region,
                econAgentIds = Map("government" -> List("government")),
                econActors = Map("government" -> govt))
              val actorRef = context.spawn(RegionActor(state), region.id)
              regions += (actorRef.path.name -> actorRef)
              govt ! GovernmentActor.AddRegion(region.id, actorRef)

              // Log event to game history
              val event = createEvent(
                region.id,
                region.id,
                EventType.Custom("RegionCreated"),
                s"A new region was established at (${location.x}, ${location.y})"
              )
              context.self ! AddGameEvent(event)

              acc :+ actorRef
            }
            replyTo ! RegionsCreated(Right(actorRefs))
          case None =>
            replyTo ! RegionsCreated(Left("cannot create regions without government"))
        }
        Behaviors.same

      case SetGovernmentInterestRate(rate, replyTo) =>
        government match {
          case Some(govtActor) =>
            govtActor ! SetInterestRate(rate)
             replyTo ! true
          case None =>
            replyTo ! false
        }
        Behaviors.same

      case SetGovernmentPrice(priceType, resourceType, price, replyTo) =>
        government match {
          case Some(govtActor) =>
            priceType.toLowerCase match {
              case "bid" =>
                context.ask(govtActor, GovernmentActor.SetBidPrice(resourceType, price, _)) {
                  case Success(_) =>
                    replyTo ! true
                    ActorNoOp()
                  case Failure(_) =>
                    replyTo ! false
                    ActorNoOp()
                }
              case "ask" =>
                context.ask(govtActor, GovernmentActor.SetAskPrice(resourceType, price, _)) {
                  case Success(_) =>
                    replyTo ! true
                    ActorNoOp()
                  case Failure(_) =>
                    replyTo ! false
                    ActorNoOp()
                }
              case _ =>
                replyTo ! false
            }
          case None =>
            replyTo ! false
        }
        Behaviors.same

      case CreateRandomRegion(x, y, replyTo) =>
        government match {
          case Some(govt) =>
            val region = Region.newRandomRegion(x, y)
            val state = RegionActorState(region = region,
              econAgentIds = Map("government" -> List("government")),
              econActors = Map("government" -> govt))
            val actorRef = context.spawn(RegionActor(state), region.id)
            regions += (actorRef.path.name -> actorRef)
            govt ! GovernmentActor.AddRegion(region.id, actorRef)

            // Log event to game history
            val event = createEvent(
              region.id,
              region.id,
              EventType.Custom("RegionCreated"),
              s"A new region was established at ($x, $y)"
            )
            context.self ! AddGameEvent(event)

            replyTo ! RegionCreated(Right(actorRef))
          case None =>
            replyTo ! RegionCreated(Left("cannot create region without government"))
        }
        Behaviors.same

      // Game history handling
      case AddGameEvent(event) =>
        // Add the event to our history
        gameHistory = gameHistory.addEvent(event)
        // Optionally log the event to console for debugging
        //println(s"Game event: $event")
        Behaviors.same

      case GetGameHistory(agentId, regionId, eventType, fromTime, toTime, offset, limit, replyTo) =>
        val (results, totalCount) = gameHistory.search(agentId, regionId, eventType, fromTime, toTime, offset, limit)
        replyTo ! GameHistoryResponse(results, totalCount)
        Behaviors.same

      // Existing message handlers...
      case GetRegionInfo(uuid, replyTo) =>
        regions.get(uuid) match {
          case Some(regionActor) =>
            println(s"manager actor pinging region ${uuid}")
            regionActor ! ShowInfo(replyTo)
          case None =>
            replyTo ! None
        }
        Behaviors.same

      case GetRegionsInfo(replyTo) =>
        if (regions.isEmpty) {
          replyTo ! List.empty[RegionActor.InfoResponse]
        } else {
          val requestId = UUID.randomUUID()
          regions.foreach { (uuid, regionActor) =>
            context.ask(regionActor, ShowInfo.apply) {
              case Success(Some(response: RegionActor.InfoResponse)) =>
                AggregateRegionsInfo(replyTo, requestId, Some(response), regions.keys.toList.length)
              case _ =>
                AggregateRegionsInfo(replyTo, requestId, None, regions.keys.toList.length)
            }
          }
        }
        Behaviors.same

      case AggregateRegionsInfo(replyTo, requestId, Some(response), expectedSize) =>
        val updated = response :: inProgressCollections.getOrElse(requestId, List())
        inProgressCollections += (requestId -> updated)
        if (updated.length == expectedSize) {
          replyTo ! updated
        }
        Behaviors.same

      case GetFullRegionInfo(uuid, replyTo) =>
        regions.get(uuid) match {
          case Some(regionActor) =>
            println(s"manager actor pinging region ${uuid}")
            regionActor ! ShowFullInfo(replyTo)
          case None =>
            replyTo ! None
        }
        Behaviors.same

      case GetRegionActor(uuid, replyTo) =>
        replyTo ! regions.get(uuid)
        Behaviors.same

      case unhandledthing =>
        println(s"getting unhandled message ${unhandledthing.toString}")
        Behaviors.same
    }
  }
}