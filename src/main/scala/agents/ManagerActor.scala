package agents

import agents.RegionActor.ShowFullInfo
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout
import middleware.GameInfo

import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

object ManagerActor {
  trait Command
  case class CreateGovernment(replyTo: ActorRef[Option[Government]]) extends Command
  case class CreateRandomRegion(replyTo: ActorRef[RegionCreated]) extends Command
  case class RegionCreated(actorRef: Either[String, ActorRef[RegionActor.Command]]) extends Command
  case class GetGovtInfo(replyTo: ActorRef[Option[Government]]) extends Command
  case class GetRegionInfo(uuid: String, replyTo: ActorRef[Option[GameInfo.InfoResponse]]) extends Command
  case class GetFullRegionInfo(uuid: String, replyTo: ActorRef[Option[GameInfo.InfoResponse]]) extends Command

  case class GetRegionActor(uuid: String, replyTo: ActorRef[Option[ActorRef[RegionActor.Command]]]) extends Command
  private case class InternalRegionResponse(regionOpt: Option[Region], replyTo: ActorRef[Option[Region]]) extends Command
  private case class InternalGovtResponse(govtOpt: Option[Government], replyTo: ActorRef[Option[Government]]) extends Command

  private var government: Option[ActorRef[GovernmentActor.Command]] = None
  private var regions = Map.empty[String, ActorRef[RegionActor.Command]]



  def apply(): Behavior[Command] = Behaviors.setup { context =>
    implicit val ec = context.executionContext
    implicit val timeout: Timeout = 3.seconds
    implicit val scheduler = context.system.scheduler
    Behaviors.receiveMessage {
      case CreateGovernment(replyTo) =>
        println("====CREATE GOVT COMMAND=====")
        government match {
          case Some(actorRef) =>
            context.ask(actorRef, ShowInfo) {
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
            context.ask(actorRef, ShowInfo) {
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
            context.ask(actorRef, ShowInfo) {
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


      case CreateRandomRegion(replyTo) =>
        government match {
          case Some(govt) =>
            val region = Region.newRandomRegion()
            val state = RegionActorState(region = region, governmentActor = govt, econActorIds = Map())
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

      case InternalRegionResponse(regionOpt, originalReplyTo) =>
        // Forward the region info to the original requester
        originalReplyTo ! regionOpt
        Behaviors.same

      case unhandledthing =>
        println(s"getting unhandled message ${unhandledthing.toString}")
        Behaviors.same
    }
  }
}
