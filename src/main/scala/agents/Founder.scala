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
                             percentComplete: Int
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

            case ReceiveBid(replyTo, bidderId, resourceType, quantity, price) =>
              // Bids here are used to goad the founder into investing
              // Initialize Producer with all necessary fields
              val facility = Producer.newProducer(founder.regionId, 1, resourceType)
                .copy(
                  maxWorkers = 10,
                  wage = 1,
                  storedResources = Map(),
                  outstandingBonds = Map()
                )

              val newSite = ConstructionSite(
                id = UUID.randomUUID.toString,
                facility = facility,
                percentComplete = 0
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

          // Extract site once at the beginning
          founder.site match {
            case Some(site) =>
              // Work with site and facility directly
              val facility = site.facility

              message match {
                case ShowInfo(replyTo) =>
                  replyTo ! Some(InfoResponse(founder))
                  Behaviors.same

                case IssueBond(sendTo, principal, interest) =>
                  val bond = Bond(UUID.randomUUID().toString, principal, interest, principal, founder.id)
                  context.ask(sendTo, ReceiveBond(bond, _, context.self)) {
                    case Success(Some(offered: Bond)) =>
                      // TODO add in negotiation, for now just accept counteroffer
                      context.self ! HireWorkers(facility.wage)
                      AddOutstandingBond(offered)
                    case _ =>
                      ActorNoOp()
                  }
                  Behaviors.same

                case AddOutstandingBond(bond) =>
                  val updatedResources = facility.storedResources +
                    (Money -> (facility.storedResources.getOrElse(Money, 0) + bond.principal))

                  val newOutstandingBonds = facility.outstandingBonds + (bond.id -> bond)

                  val updatedFacility = facility.copy(
                    storedResources = updatedResources,
                    outstandingBonds = newOutstandingBonds
                  )

                  construction(state.copy(
                    founder = founder.copy(
                      site = Some(site.copy(facility = updatedFacility)))))

                case PayBond(bond, amount, replyTo) =>
                  val amountToPay = Math.min(facility.storedResources.getOrElse(Money, 0), amount)
                  val updatedBond = bond.copy(totalOutstanding = Math.round((bond.totalOutstanding - amountToPay) * bond.interestRate).toInt)
                  val newOutstandingBonds = if (updatedBond.totalOutstanding <= 0) {
                    facility.outstandingBonds - bond.id
                  } else {
                    facility.outstandingBonds + (bond.id -> updatedBond)
                  }

                  val updatedResources = facility.storedResources +
                    (Money -> (facility.storedResources.getOrElse(Money, 0) - amountToPay))

                  val updatedFacility = facility.copy(
                    storedResources = updatedResources,
                    outstandingBonds = newOutstandingBonds
                  )

                  replyTo ! amountToPay
                  construction(state.copy(
                    founder = founder.copy(
                      site = Some(site.copy(facility = updatedFacility)))))

                case HireWorkers(wage) =>
                  if (facility.workers >= facility.maxWorkers) {
                    // Do nothing
                    Behaviors.same
                  } else if (facility.storedResources.getOrElse(Money, 0) < facility.maxWorkers * facility.wage) {
                    // Would prefer to do this with the ask pattern but IssueBond may take multiple tries after which the callback is lost
                    // So instead IssueBond takes care of going back into the loop of hiring workers
                    context.self ! IssueBond(state.regionActor, facility.maxWorkers * facility.wage, 0.01)
                    Behaviors.same
                  } else {
                    context.ask(state.regionActor, RegionActor.ReceiveWorkerBid(_, state.founder.id, facility.wage)) {
                      case Success(Right(())) =>
                        AddWorker()
                      case Success(Left(Some(co: Int))) =>
                        context.self ! SetWage(co)
                        context.self ! HireWorkers(co)
                        ActorNoOp()
                      case _ =>
                        ActorNoOp()
                    }
                    Behaviors.same
                  }

                case ProgressConstruction() =>
                  context.self ! PayWages()

                  val percentComplete = site.percentComplete
                  val newProgress = Math.min(percentComplete + facility.workers, 100)

                  if (facility.workers < facility.maxWorkers) {
                    context.self ! HireWorkers(facility.wage)
                  }

                  if (newProgress >= 100) {
                    context.self ! CompleteConstruction()
                  }
                  construction(state.copy(founder = founder.copy(
                    site = Some(site.copy(percentComplete = newProgress)))))

                case AddWorker() =>
                  val updatedFacility = facility.copy(workers = facility.workers + 1)
                  construction(state.copy(founder = founder.copy(
                    site = Some(site.copy(facility = updatedFacility)))))

                case SetWage(wage) =>
                  val updatedFacility = facility.copy(wage = wage)
                  construction(state.copy(founder = founder.copy(
                    site = Some(site.copy(facility = updatedFacility)))))

                case PayWages() =>
                  val workersToPay = Math.min(Math.floorDiv(facility.storedResources.getOrElse(Money, 0), facility.wage), facility.workers)
                  if (workersToPay < facility.workers) {
                    state.regionActor ! UnassignWorkers(facility.workers - workersToPay)
                  }
                  val updatedResources = facility.storedResources +
                    (Money -> (facility.storedResources.getOrElse(Money, 0) - (workersToPay * facility.wage)))

                  val updatedFacility = facility.copy(
                    workers = workersToPay,
                    storedResources = updatedResources
                  )

                  construction(state.copy(founder = founder.copy(
                    site = Some(site.copy(facility = updatedFacility)))))

                case CompleteConstruction() =>
                  // The facility already has all the necessary fields
                  state.regionActor ! BuildProducer(facility)
                  Behaviors.stopped

                case _ =>
                  Behaviors.same
              }

            case None =>
              // Handle the case where there's no site
              message match {
                case ShowInfo(replyTo) =>
                  replyTo ! Some(InfoResponse(founder))
                  Behaviors.same
                case _ =>
                  Behaviors.same
              }
          }
        }
      }

      preConstruction(state)
    }
  }
}