package agents

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout
import middleware.{EventType, GameEvent, GameEventService, GameInfo}

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
    // Create event service
    val eventService = GameEventService(context)

    Behaviors.withTimers { timers =>
      def tick(state: BankActorState): Behavior[Command] = {
        val bank = state.bank

        // Helper function for logging events
        def logBankEvent(eventType: EventType, eventText: String): Unit = {
          eventService.logEvent(
            agentId = bank.id,
            regionId = bank.regionId,
            eventType = eventType,
            eventText = eventText
          )
        }

        Behaviors.receive { (context, message) =>
          message match {
            case ShowInfo(replyTo) =>
              replyTo ! Some(InfoResponse(bank))
              Behaviors.same

            case ReceiveBond(bond, replyTo, issuedFrom) =>
              // TODO factor in reserve requirements--for now, there are none
              // as of right now, it will simply borrow any money needed to buy the bond from the government (central bank)
              // will deal with more serious constraints later
              logBankEvent(
                EventType.Custom("BondReceived"),
                s"Received bond ${bond.id} from ${bond.debtorId} with principal ${bond.principal}"
              )

              if (bank.storedMoney < bond.principal) {
                logBankEvent(
                  EventType.Custom("IssuingBond"),
                  s"Insufficient funds (${bank.storedMoney}) to cover bond principal (${bond.principal}). Issuing new bond."
                )
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
              logBankEvent(
                EventType.BondIssued,
                s"Issued bond ${bond.id} with principal $principal at ${interest * 100}% interest"
              )

              context.ask(sendTo, ReceiveBond(bond, _, context.self)) {
                case Success(Some(offered: Bond)) =>
                  AddOutstandingBond(offered) // for now, just acccept whatever the counteroffer is
                case _ =>
                  ActorNoOp()
              }
              Behaviors.same

            case PayBond(bond, amount, replyTo) =>
              val amountToPay = Math.min(bank.storedMoney, amount) // Pay what it can without defaults

              if (amountToPay < amount) {
                logBankEvent(
                  EventType.Custom("PartialPayment"),
                  s"Making partial bond payment: $amountToPay of requested $amount"
                )
              } else {
                logBankEvent(
                  EventType.BondRepaid,
                  s"Making bond payment of $amountToPay to bond ${bond.id}"
                )
              }

              val updatedBond = bond.copy(totalOutstanding = Math.round((bond.totalOutstanding - amountToPay) * bond.interestRate).toInt)
              val newOutstandingBonds = if (updatedBond.totalOutstanding <= 0) {
                logBankEvent(
                  EventType.BondRepaid,
                  s"Bond ${bond.id} fully repaid"
                )
                bank.outstandingBonds - bond.id
              } else {
                bank.outstandingBonds + (bond.id -> updatedBond)
              }

              // Update the bank's stored money
              val newStoredMoney = bank.storedMoney - amountToPay

              // Reply with the amount paid
              replyTo ! amountToPay

              tick(state = state.copy(bank = bank.copy(
                storedMoney = newStoredMoney,
                outstandingBonds = newOutstandingBonds
              )))

            case AddOutstandingBond(bond) =>
              val newOutstandingBonds = bank.outstandingBonds + (bond.id -> bond)
              logBankEvent(
                EventType.Custom("BondFunded"),
                s"Bond ${bond.id} funded with ${bond.principal}. Bank funds increased from ${bank.storedMoney} to ${bank.storedMoney + bond.principal}"
              )

              tick(state = state.copy(bank = bank.copy(
                storedMoney = (bank.storedMoney + bond.principal),
                outstandingBonds = newOutstandingBonds)))

            case CollectBondPayment(bondId, amount) =>
              // Look up the current version of the bond from bondsOwned
              bank.bondsOwned.get(bondId) match {
                case Some(currentBond) =>
                  logBankEvent(
                    EventType.Custom("CollectingPayment"),
                    s"Collecting payment of $amount on bond ${currentBond.id} from ${currentBond.debtorId}"
                  )

                  state.econActors.get(currentBond.debtorId) match {
                    case Some(actorRef) =>
                      context.ask(actorRef, PayBond(currentBond, amount, _)) {
                        case Success(payment: Int) =>
                          DepositBondPayment(currentBond, payment)
                        case Failure(err) =>
                          logBankEvent(
                            EventType.Custom("PaymentFailed"),
                            s"Failed to collect payment on bond ${currentBond.id}: ${err.getMessage}"
                          )
                          ActorNoOp()
                        case _ =>
                          ActorNoOp()
                      }
                    case None =>
                      logBankEvent(
                        EventType.Custom("DebtorNotFound"),
                        s"Could not find actor reference for debtor ${currentBond.debtorId}"
                      )
                  }
                case None =>
                  logBankEvent(
                    EventType.Custom("BondNotFound"),
                    s"Bond $bondId no longer exists in bonds owned"
                  )
                  timers.cancel(s"collect-$bondId")
              }
              Behaviors.same

            case DepositBondPayment(bond, amount) =>
              val newStoredMoney = bank.storedMoney + amount
              val updatedBond = bond.copy(totalOutstanding = ((bond.totalOutstanding - amount)*bond.interestRate).toInt)

              if (updatedBond.totalOutstanding <= 0) {
                logBankEvent(
                  EventType.BondRepaid,
                  s"Bond ${bond.id} fully repaid by ${bond.debtorId}. Final payment: $amount"
                )
                timers.cancel(s"collect-${bond.id}")
              } else {
                logBankEvent(
                  EventType.Custom("PaymentReceived"),
                  s"Received payment of $amount on bond ${bond.id}. Remaining balance: ${updatedBond.totalOutstanding}"
                )
              }

              val updatedBonds = if (updatedBond.totalOutstanding <= 0) {
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
                    logBankEvent(
                      EventType.Custom("DebtorUpdated"),
                      s"Updated debtor for bond $bondId from ${bond.debtorId} to $newDebtorId"
                    )
                    bonds + (bondId -> bond.copy(debtorId = newDebtorId))
                  case None =>
                    bonds
                }
              }

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