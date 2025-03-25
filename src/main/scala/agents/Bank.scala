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
               debtorId: String
)

case class BankActorState(
                           bank: Bank,
                           regionActor: ActorRef[RegionActor.Command],
                           econActors: Map[String, EconActor],
                         )

case class Bank(
               id: String,
               regionId: String,
               storedMoney: Int,
               interestRate: Double,
               bondsOwned: Map[String, Bond],
               outstandingBonds: Map[String, Bond]
               ) extends EconAgent

object Bank {
  def newBank(regionId: String): Bank = {
    Bank(
      id = UUID.randomUUID().toString,
      regionId = regionId,
      storedMoney = 0,
      interestRate = 0.05,
      bondsOwned = Map(),
      outstandingBonds = Map()
    )
  }
}

object BankActor {

  type Command = BankingCommand | GameActorCommand | EconAgent.Command

  case class InfoResponse(bank: Bank) extends GameInfo.InfoResponse {
    override val agent: Bank = bank
  }

  implicit val timeout: Timeout = Timeout(3.seconds) // Define an implicit timeout for ask pattern

  def apply(state: BankActorState): Behavior[Command] = Behaviors.setup { context =>
    Behaviors.withTimers { timers =>
      def tick(state: BankActorState): Behavior[Command] = {
        val bank = state.bank
        Behaviors.receive { (context, message) =>
          message match {
            case ShowInfo(replyTo) =>
              replyTo ! Some(InfoResponse(bank))
              Behaviors.same

            case ReceiveBond(bond, replyTo, issuedFrom) =>
              // TODO factor in reserve requirements--for now, there are none
              // as of right now, it will simply borrow any money needed to buy the bond from the government (central bank)
              // will deal with more serious constraints later
              println(s"====BANK ${bank.id} RECEIVING BOND FROM ${bond.debtorId}")
              if (bank.storedMoney < bond.principal) {
                println(s"===BANK HAS ${bank.storedMoney} BUT PRINCIPAL IS ${bond.principal}, ISSUING BOND===")
                // borrow money to cover cost
                // TODO additional money will come from deposits by region/producers, but for now this will suffice
                context.self ! IssueBond(state.regionActor, bond.principal, bank.interestRate * 0.9)
              }
              replyTo ! Some(bond.copy(interestRate = bank.interestRate)) // TODO consider risks of having ID mess up matching
              timers.startTimerWithFixedDelay(s"collect-${bond.id}", CollectBondPayment(bond.id, Math.round(bond.principal/10)), 20.second)
              tick(state.copy(
                econActors = state.econActors + (bond.debtorId -> issuedFrom),
                bank = bank.copy(
                  bondsOwned = bank.bondsOwned + (bond.id -> bond),
                  storedMoney = bank.storedMoney - bond.principal
                )))

            case IssueBond(sendTo, principal, interest) =>
              val bond = Bond(UUID.randomUUID().toString, principal, interest, principal, bank.id)
              context.ask(sendTo, ReceiveBond(bond, _, context.self)) {
                case Success(Some(offered: Bond)) =>
                  AddOutstandingBond(offered) // for now, just acccept whatever the counteroffer is
                case _ =>
                  ActorNoOp()
              }
              Behaviors.same

            case AddOutstandingBond(bond) =>
              val newOutstandingBonds = bank.outstandingBonds + (bond.id -> bond)
              println(s"===BANK RECEIVING ${bond.principal}, FUNDS NOW GOING FROM ${bank.storedMoney} to ${bank.storedMoney + bond.principal}===")
              tick(state = state.copy(bank = bank.copy(
                storedMoney = (bank.storedMoney + bond.principal),
                outstandingBonds = newOutstandingBonds)))

            case CollectBondPayment(bondId, amount) =>
              // Look up the current version of the bond from bondsOwned
              bank.bondsOwned.get(bondId) match {
                case Some(currentBond) =>
                  state.econActors.get(currentBond.debtorId) match {
                    case Some(actorRef) =>
                      context.ask(actorRef, PayBond(currentBond, amount, _)) {
                        case Success(payment: Int) =>
                          DepositBondPayment(currentBond, payment)
                        case Failure(err) =>
                          ActorNoOp()
                        case _ =>
                          ActorNoOp()
                      }
                    case None =>
                      println(s"====COULDN'T FIND ACTOR REF FOR DEBTOR ${currentBond.debtorId}====")
                  }
                case None =>
                  println(s"====BOND $bondId NO LONGER EXISTS IN BONDSOWNED====")
                  timers.cancel(s"collect-$bondId")
              }
              Behaviors.same

            case DepositBondPayment(bond, amount) =>
              val newStoredMoney = bank.storedMoney + amount
              val updatedBond = bond.copy(totalOutstanding = ((bond.totalOutstanding - amount)*bond.interestRate).toInt)
              val updatedBonds = if (updatedBond.totalOutstanding <= 0) {
                timers.cancel(s"collect-${bond.id}")
                bank.bondsOwned - bond.id
              } else {
                bank.bondsOwned + (bond.id -> updatedBond)
              }
              tick(state = state.copy(bank = bank.copy(storedMoney = newStoredMoney, bondsOwned = updatedBonds)))

            case UpdateBondDebtor(bondIds, newDebtorId, newDebtorActor) =>
              // First, update the bonds with the new debtor ID
              val updatedBondsOwned = bondIds.foldLeft(bank.bondsOwned) { (bonds, bondId) =>
                bonds.get(bondId) match {
                  case Some(bond) =>
                    // Update the bond's debtor ID
                    bonds + (bondId -> bond.copy(debtorId = newDebtorId))
                  case None =>
                    bonds
                }
              }

              println("=================")
              println(s"original bonds owned: ${bank.bondsOwned}")
              println(s"updated bonds owned ${updatedBondsOwned}")
              println("=================")

              // Update the econActors mapping to point to the new debtor
              val updatedEconActors = bondIds.foldLeft(state.econActors) { (actors, bondId) =>
                bank.bondsOwned.get(bondId) match {
                  case Some(bond) if actors.contains(bond.debtorId) =>
                    // Remove old mapping and add new one with the explicit actor reference
                    actors - bond.debtorId + (newDebtorId -> newDebtorActor)
                  case _ => actors
                }
              }

              tick(state.copy(
                bank = bank.copy(bondsOwned = updatedBondsOwned),
                econActors = updatedEconActors
              ))

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