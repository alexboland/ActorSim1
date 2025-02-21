package agents

import agents.Founder.*
import agents.RegionActor.BuildProducer
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout

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
                       wage: Int
                       )

object Founder {
  trait Command

  case class HireWorkers() extends Command

  case class ProgressConstruction() extends Command

  case class CompleteConstruction() extends Command

  case class AddWorker() extends Command // There's another command with this name for ResourceProducer but I want to make this separate here for now
}

object FounderActor {
  type Command = EconAgent.Command | Founder.Command | GameActorCommand

  implicit val timeout: Timeout = Timeout(3.seconds) // Define an implicit timeout for ask pattern


  def apply(state: FounderActorState): Behavior[Command] = Behaviors.setup { context =>
    preConstruction(state)
  }

  private def preConstruction(state: FounderActorState): Behavior[Command] = {
    Behaviors.receive { (context, message) =>
      val founder = state.founder
      message match {
        case ReceiveBid(replyTo, resourceType, quantity, price) =>
          // Bids here are used to goad the founder into investing
          // Need to figure out the best way to map resourceType to type of facility to build
          val newSite = ConstructionSite(
            id = UUID.randomUUID.toString,
            facility = Producer.newProducer(founder.regionId, 1, resourceType),
            percentComplete = 0,
            workers = 0,
            maxWorkers = 10,
            wage = 1 // For now will just keep incrementing, but consider some kind of pinging to get this information
          )

          construction(state.copy(founder = founder.copy(site = Some(newSite))))

        case _ =>
          Behaviors.same
      }
    }
  }

  private def construction(state: FounderActorState): Behavior[Command] = {
    Behaviors.withTimers { timers =>
      timers.startTimerWithFixedDelay("construction", ProgressConstruction(), 2.second)

      Behaviors.receive { (context, message) =>
        val founder = state.founder
        message match {
          // TODO figure out financing behavior on a number of levels:
          // 1. we want to send this to the region which will forward it to the bank (for the purposes of this prototype)
          // 2. given that the regional bank will have a specific interest rate, we don't want to have to have endless counteroffers
          // but perhaps utilizing counteroffers similar to what i built with other bids makes the most sense
          // 3. we need to make sure to implement forwarding to/from the regional bank in the reigon actor
          case IssueBond(principal, interestRate, issueTo) =>
            Behaviors.same


          case HireWorkers() =>
            founder.site match {
              case Some(site) =>
                context.ask(state.regionActor, RegionActor.ReceiveWorkerBid(_, state.founder.id, site.wage)) {
                  case Success(true) =>
                    AddWorker()
                  case _ =>
                    ActorNoOp()
                }
                Behaviors.same
              case None =>
                Behaviors.same
            }

          case ProgressConstruction() =>
            founder.site match {
              case Some(site) =>
                // TODO pay worker wages in this loop as well
                val percentComplete = site.percentComplete
                val newProgress = Math.min(percentComplete + site.workers, 100)

                if (site.workers < site.maxWorkers) {
                  context.self ! HireWorkers()
                }

                if (newProgress >= 100) {
                  context.self ! CompleteConstruction()
                }
                construction(state.copy(founder = founder.copy(site = Some(site.copy(percentComplete = newProgress)))))
              case _ =>
                Behaviors.same
            }

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
  }
}