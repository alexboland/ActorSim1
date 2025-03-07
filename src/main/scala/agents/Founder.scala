package agents

import agents.Founder.*
import agents.RegionActor.{BuildProducer, UnassignWorkers}
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout
import middleware.GameInfo

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.util.Success

case class FounderActorState(
                           founder: Founder,
                           regionActor: ActorRef[RegionActor.Command]
                         )

case class Founder(
                    id: String,
                    regionId: String,
                    site: Option[ConstructionSite]
                  ) extends EconAgent

case class ConstructionSite(
                       id: String,
                       facility: Producer,
                       percentComplete: Int,
                       workers: Int,
                       maxWorkers: Int,
                       wage: Int,
                       storedResources: Map[ResourceType, Int], // for now just need money but keeping it flexible
                       outstandingBonds: Map[String, Bond]
                       )

object Founder {
  trait Command

  case class HireWorkers(wage: Int) extends Command

  case class ProgressConstruction() extends Command

  case class CompleteConstruction() extends Command

  case class AddWorker() extends Command // There's another command with this name for ResourceProducer but I want to make this separate here for now

  case class PayWages() extends Command

  case class SetWage(wage: Int) extends Command
}

object FounderActor {
  type Command = EconAgent.Command | Founder.Command | GameActorCommand

  case class InfoResponse(founder: Founder) extends GameInfo.InfoResponse {
    override val agent: Founder = founder
  }

  implicit val timeout: Timeout = Timeout(3.seconds) // Define an implicit timeout for ask pattern


  def apply(state: FounderActorState): Behavior[Command] = Behaviors.setup { context =>
    Behaviors.withTimers { timers =>
      def preConstruction(state: FounderActorState): Behavior[Command] = {
        Behaviors.receive { (context, message) =>
          val founder = state.founder
          message match {
            case ShowInfo(replyTo) =>
              replyTo ! Some(InfoResponse(founder))
              Behaviors.same

            case ReceiveBid(replyTo, resourceType, quantity, price) =>
              // Bids here are used to goad the founder into investing
              // Need to figure out the best way to map resourceType to type of facility to build
              val newSite = ConstructionSite(
                id = UUID.randomUUID.toString,
                facility = Producer.newProducer(founder.regionId, 1, resourceType),
                percentComplete = 0,
                workers = 0,
                maxWorkers = 10,
                wage = 1, // For now will just keep incrementing, but consider some kind of pinging to get this information
                storedResources = Map(),
                outstandingBonds = Map(),
              )

              timers.startTimerWithFixedDelay("construction", ProgressConstruction(), 2.second)
              construction(state.copy(founder = founder.copy(site = Some(newSite))))

            case _ =>
              Behaviors.same
          }
        }
      }

      def construction(state: FounderActorState): Behavior[Command] = {
        Behaviors.receive { (context, message) =>
          val founder = state.founder
          // TODO refactor by having the site option matched for in a way that wraps around the message match
          message match {
            case ShowInfo(replyTo) =>
              replyTo ! Some(InfoResponse(founder))
              Behaviors.same

            case IssueBond(sendTo, principal, interest) =>
              val bond = Bond(UUID.randomUUID().toString, principal, interest, principal, founder.id)
              context.ask(sendTo, ReceiveBond(bond, _, context.self)) {
                case Success(Some(offered: Bond)) =>
                  if (bond == offered) {
                    founder.site.map { site =>
                      context.self ! HireWorkers(site.wage) // Kind of hacky but will work for now: will re-evaluate hiring whenever a bond clears
                      AddOutstandingBond(bond)
                    }.getOrElse(ActorNoOp())
                  } else {
                    IssueBond(sendTo, offered.principal, offered.interestRate) // Repeat but with their counteroffer
                  }
                case _ =>
                  ActorNoOp()
              }
              Behaviors.same


            case HireWorkers(wage) =>
              founder.site.foreach { site =>
                if (site.workers >= site.maxWorkers) {
                  ActorNoOp()
                } else if (site.storedResources.getOrElse(Money, 0) < site.maxWorkers * site.wage) {
                  // Would prefer to do this with the ask pattern but IssueBond may take multiple tries after which the callback is lost
                  // So instead IssueBond takes care of going back into the loop of hiring workers
                  context.self ! IssueBond(state.regionActor, site.maxWorkers * site.wage, 0.01)
                } else {
                  context.ask(state.regionActor, RegionActor.ReceiveWorkerBid(_, state.founder.id, site.wage)) {
                    case Success(Right(())) =>
                      AddWorker()
                    case Success(Left(Some(co: Int))) =>
                      context.self ! SetWage(co)
                      context.self ! HireWorkers(co)
                      ActorNoOp()
                    case _ =>
                      ActorNoOp()
                  }
                }
              }
              Behaviors.same

            case ProgressConstruction() =>
              founder.site match {
                case Some(site) =>
                  context.self ! PayWages()

                  val percentComplete = site.percentComplete
                  val newProgress = Math.min(percentComplete + site.workers, 100)

                  if (site.workers < site.maxWorkers) {
                    context.self ! HireWorkers(site.wage)
                  }

                  if (newProgress >= 100) {
                    context.self ! CompleteConstruction()
                  }
                  construction(state.copy(founder = founder.copy(site = Some(site.copy(percentComplete = newProgress)))))
                case _ =>
                  Behaviors.same
              }

            case SetWage(wage) =>
              founder.site.map { site =>
                construction(state.copy(founder = founder.copy(site = Some(site.copy(wage = wage)))))
              }.getOrElse(Behaviors.same)

            case PayWages() =>
              founder.site.map { site =>
                val workersToPay = Math.min(Math.floorDiv(site.storedResources.getOrElse(Money, 0), site.wage), site.workers)
                if (workersToPay < site.workers) {
                  state.regionActor ! UnassignWorkers(site.workers - workersToPay)
                }
                val updatedResources = site.storedResources +
                  (Money -> (site.storedResources.getOrElse(Money, 0) - (workersToPay * site.wage)))
                construction(state.copy(founder = founder.copy(site =
                  Some(site.copy(workers = workersToPay, storedResources = updatedResources)))))
              }.getOrElse(Behaviors.same)


            case CompleteConstruction() =>
              founder.site match {
                case Some(site) =>
                  state.regionActor ! BuildProducer(site.facility)
                case _ =>
                // This definitely shouldn't happen
              }
              Behaviors.stopped

            case _ =>
              Behaviors.same
          }
        }
      }
      preConstruction(state)

    }
  }
}