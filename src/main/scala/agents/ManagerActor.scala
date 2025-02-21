package agents

import agents.RegionActor.ShowFullInfo
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout
import middleware.GameInfo

import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, DurationInt}
import scala.util.{Failure, Success}
import akka.actor.typed.scaladsl.AskPattern.Askable

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

  private case class InternalRegionResponse(regionOpt: Option[Region]) extends Command
  private case class InternalGovtResponse(govtOpt: Option[Government], replyTo: ActorRef[Option[Government]]) extends Command



  private var government: Option[ActorRef[GovernmentActor.Command]] = None
  private var regions = Map.empty[String, ActorRef[RegionActor.Command]]

  private var inProgressCollections = Map.empty[UUID, List[RegionActor.InfoResponse]]

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
                InternalGovtResponse(Some(govt), replyTo)//Just do it without an error message for now and make it idempotent
              case Failure(f) =>
                println(s"failure due to ${f.toString}")
                InternalGovtResponse(None, replyTo)
            }

          case None =>
            println("====NO GOVT FOUND=====")
            val actorRef = context.spawn(GovernmentActor(), "Government")
            government = Some(actorRef)
            actorRef ! GovernmentActor.InitializeGov(Government.newGov())
            context.ask(actorRef, ShowInfo.apply) {
              case Success(Some(GovernmentActor.InfoResponse(govt))) =>
                println("success with govt ping!")
                InternalGovtResponse(Some(govt), replyTo) //Just do it without an error message for now and make it idempotent
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
                InternalGovtResponse(Some(govt), replyTo) //Just do it without an error message for now and make it idempotent
              case Failure(f) =>
                println(s"failure due to ${f.toString}")
                InternalGovtResponse(None, replyTo)
            }
          case None =>
            InternalGovtResponse(None, replyTo)
        }
        Behaviors.same


      case InternalGovtResponse(govtOpt, originalReplyTo) =>
        // Forward the region info to the original requester
        originalReplyTo ! govtOpt
        Behaviors.same


      case CreateRegions(locations, replyTo) =>
        government match {
          case Some(govt) =>
            val actorRefs = locations.foldLeft(List[ActorRef[RegionActor.Command]]()) { (acc, location) =>
              val region = Region.newRandomRegion(location.x, location.y)
              val state = RegionActorState(region = region, governmentActor = govt, econActors = Map())
              val actorRef = context.spawn(RegionActor(state), region.id)
              regions += (actorRef.path.name -> actorRef)
              govt ! GovernmentActor.AddRegion(region.id, actorRef)
              acc :+ actorRef
              }
            replyTo ! RegionsCreated(Right(actorRefs))
          case None =>
            replyTo ! RegionsCreated(Left("cannot create regions without government"))
        }
        Behaviors.same

      case CreateRandomRegion(x, y, replyTo) =>
        government match {
          case Some(govt) =>
            val region = Region.newRandomRegion(x, y)
            val state = RegionActorState(region = region, governmentActor = govt, econActors = Map())
            val actorRef = context.spawn(RegionActor(state), region.id)
            regions += (actorRef.path.name -> actorRef)
            govt ! GovernmentActor.AddRegion(region.id, actorRef)
            replyTo ! RegionCreated(Right(actorRef))
          case None =>
            replyTo ! RegionCreated(Left("cannot create region without government"))

        }
        Behaviors.same

      case GetRegionInfo(uuid, replyTo) =>
        regions.get(uuid) match {
          case Some(regionActor) =>
            // Ask the RegionActor for info and forward the response
            println(s"manager actor pinging region ${uuid}")
            regionActor ! ShowInfo(replyTo)
          case None =>
            // Region not found, reply with None
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
              case Success(Some(response: RegionActor.InfoResponse)) => AggregateRegionsInfo(replyTo, requestId, Some(response), regions.keys.toList.length)
              case _ => AggregateRegionsInfo(replyTo, requestId, None, regions.keys.toList.length) // TODO figure out error handling later
            }
          }
        }

        Behaviors.same

      case AggregateRegionsInfo(replyTo, requestId, Some(response), expectedSize) =>
        val updated = response :: inProgressCollections.getOrElse(requestId, List())
        inProgressCollections += (requestId -> updated)
        if (updated.length == expectedSize) {
          //inProgressCollections -= requestId
          replyTo ! updated
        }
        Behaviors.same


      case GetFullRegionInfo(uuid, replyTo) =>
        regions.get(uuid) match {
          case Some(regionActor) =>
            // Ask the RegionActor for info and forward the response
            println(s"manager actor pinging region ${uuid}")
            regionActor ! ShowFullInfo(replyTo)
          case None =>
            // Region not found, reply with None
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
