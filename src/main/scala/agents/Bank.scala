package agents

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout
import middleware.GameInfo

import java.util.UUID
import scala.concurrent.duration.DurationInt

case class Bond(
               principal: Int,
               interestRate: Double,
               creditorId: String,
               debtorId: String
)

case class BankActorState(
                           bank: Bank,
                           econActors: Map[String, ActorRef[ResourceProducerCommand]],
                         )

case class Bank(
               id: String,
               storedMoney: Int,
               interestRate: Double,
               bonds: Map[String, Bond]
               ) extends EconAgent

object Bank {
  def newBank(): Bank = {
    Bank(
      id = UUID.randomUUID().toString,
      storedMoney = 0,
      interestRate = 0.05,
      bonds = Map()
    )
  }
}

object BankActor {

  trait Command extends EconActorCommand

  case class SetInterestRate(rate: Double) extends Command

  case class ReceiveDeposit(amount: Int)

  case class InfoResponse(bank: Bank) extends GameInfo.InfoResponse {
    override val agent = bank
  }

  def apply(state: BankActorState): Behavior[Command] = Behaviors.setup { context =>
    implicit val timeout: Timeout = Timeout(3.seconds) // Define an implicit timeout for ask pattern
    def tick(state: BankActorState): Behavior[Command] = {
      Behaviors.receive { (context, message) =>
        message match {
          case ShowInfo(replyTo) =>
            replyTo ! Some(InfoResponse(state.bank))
            Behaviors.same

          case ReceiveBond(bond, replyTo, issuedFrom) =>
            //For now just accept all bonds
            replyTo ! true //TODO consider making this not use the ask pattern so that replyTo can be the same as issuedFrom
            tick(state.copy(econActors = state.econActors + (bond.debtorId -> issuedFrom)))

          case _ =>
            Behaviors.same
        }
      }
    }

    Behaviors.withTimers { timers =>
      // TODO create loop for adjusting outstanding loans and possibly modifying its interest rates based on some kind of information
      // timers.startTimerWithFixedDelay("production", ProduceResource(), 8.second)

      tick(state)
    }
  }
}