package agents

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout
import middleware.GameInfo

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

case class Bond(
               id: String,
               principal: Int,
               interestRate: Double,
               totalOutstanding: Int,
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

  type Command = BankingCommand



  case class InfoResponse(bank: Bank) extends GameInfo.InfoResponse {
    override val agent: Bank = bank
  }

  def apply(state: BankActorState): Behavior[Command] = Behaviors.setup { context =>
    implicit val timeout: Timeout = Timeout(3.seconds) // Define an implicit timeout for ask pattern
    Behaviors.withTimers { timers =>
      def tick(state: BankActorState): Behavior[Command] = {
        Behaviors.receive { (context, message) =>
          message match {
            case ShowInfo(replyTo) =>
              replyTo ! Some(InfoResponse(state.bank))
              Behaviors.same

            case ReceiveBond(bond, replyTo, issuedFrom) =>
              //For now just accept all bonds
              replyTo ! true //TODO consider making this not use the ask pattern so that replyTo can be the same as issuedFrom
              timers.startTimerWithFixedDelay(s"collect-${bond.id}", CollectBondPayment(bond, Math.round(bond.principal/10)), 20.second)
              tick(state.copy(
                econActors = state.econActors + (bond.debtorId -> issuedFrom),
                bank = state.bank.copy(bonds = state.bank.bonds + (bond.id -> bond))))

            case CollectBondPayment(bond, amount) =>
              state.econActors.get(bond.debtorId) match {
                case Some(actorRef) =>
                  context.ask(actorRef, PayBond(bond, amount, _)) {
                    case Success(payment: Int) =>
                      DepositBondPayment(bond, payment)
                    case Failure(err) =>
                      println(s"failure in command CollectBondPayment in bank ${state.bank.id}: ${err}")
                      ActorNoOp()
                    case _ =>
                      ActorNoOp()
                  }
                case _ =>
                  println(s"couldn't find actor ref for debtor ${bond.debtorId}")
              }
              Behaviors.same

            case DepositBondPayment(bond, amount) =>
              val newStoredMoney = state.bank.storedMoney + amount
              val updatedBond = bond.copy(totalOutstanding = ((bond.totalOutstanding - amount)*bond.interestRate).toInt)
              val updatedBonds = if (bond.totalOutstanding <= 0) {
                timers.cancel(s"collect-${bond.id}")
                state.bank.bonds - bond.id
              } else {
                state.bank.bonds + (bond.id -> updatedBond)
              }
              tick(state = state.copy(bank = state.bank.copy(storedMoney = newStoredMoney, bonds = updatedBonds)))
            case _ =>
              Behaviors.same
          }
        }
      }


      // TODO create loop for adjusting outstanding loans and possibly modifying its interest rates based on some kind of information
      // timers.startTimerWithFixedDelay("production", ProduceResource(), 8.second)

      tick(state)
    }
  }
}