package agents

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout
import middleware.GameInfo

import scala.concurrent.duration.DurationInt
import java.util.UUID
import scala.util.{Failure, Success}

// TODO delegate regions and econActors maps to separate "state" struct
case class Government(
                       id: String,
                       storedResources: Map[ResourceType, Int],
                       askPrices: Map[ResourceType, Int],
                       bidPrices: Map[ResourceType, Int],
                       regions: Map[String, ActorRef[RegionActor.Command]],
                       econActors: Map[String, EconActor], // in practice this is just for other banks, but being flexible
                       bonds: Map[String, Bond],
                       interestRate: Double
                     ) extends GameAgent

object Government {
  def newGov(): Government = Government(UUID.randomUUID().toString, Map(), Map(Food -> 1), Map(Food -> 1), Map(), Map(), Map(), 0.05)
}

object GovernmentActor {
  trait GovtCommand

  case class InitializeGov(government: Government) extends GovtCommand

  case class SetBidPrice(resourceType: ResourceType, price: Int, replyTo: ActorRef[Boolean]) extends GovtCommand

  case class SetAskPrice(resourceType: ResourceType, price: Int, replyTo: ActorRef[Boolean]) extends GovtCommand

  case class InfoResponse(government: Government) extends GameInfo.InfoResponse {
    val agent: Government = government
  }

  case class UpdateBond(updatedBond: Bond) extends GovtCommand

  case class AddRegion(uuid: String, ref: ActorRef[RegionActor.Command]) extends GovtCommand

  case class BuyAndSellResources() extends GovtCommand

  type Command = GovtCommand | BankingCommand | GameActorCommand | EconAgent.Command

  implicit val timeout: Timeout = Timeout(3.seconds) // Define an implicit timeout for ask pattern

  def apply(): Behavior[Command] = Behaviors.setup { context =>

    Behaviors.withTimers { timers =>
      def tick(government: Government): Behavior[Command] = {
        val storedResources = government.storedResources

        Behaviors.receive { (context, message) =>
          message match {
            case ShowInfo(replyTo) =>
              println("government actor got ShowInfo command")
              replyTo ! Some(InfoResponse(government))
              println("government actor replied to ShowInfo command")
              Behaviors.same

            case BuyFromSeller(seller, resourceType, qty, price) =>
              val newStoredResources = storedResources + (resourceType -> (storedResources.getOrElse(resourceType, 0) + qty))
              seller ! SellToBuyer(context.self, resourceType, qty, price)

              tick(government.copy(storedResources = newStoredResources))

            case SellToBuyer(buyer, resourceType, qty, price)  =>
              val newStoredResources = storedResources + (resourceType -> (storedResources.getOrElse (resourceType, 0) - qty) )

              tick (government.copy (storedResources = newStoredResources) )

            case GetBidPrice(replyTo, resourceType) =>
              replyTo ! government.bidPrices.get(resourceType)
              Behaviors.same

            case GetAskPrice(replyTo, resourceType) =>
              replyTo ! government.askPrices.get(resourceType)
              Behaviors.same

            case SetBidPrice(resourceType, price, replyTo) =>
              val newBidPrices = government.bidPrices + (resourceType -> price)
              replyTo ! true
              tick(government.copy(bidPrices = newBidPrices))

            case SetAskPrice(resourceType, price, replyTo) =>
              val newAskPrices = government.askPrices + (resourceType -> price)
              replyTo ! true
              tick(government.copy(askPrices = newAskPrices))

            case BuyAndSellResources() =>
              government.regions.foreach { (regionUuid, regionActor) =>
                government.bidPrices.foreach { (rt, price) =>
                  context.self ! MakeBid(regionActor, rt, 10, price) // TODO figure out quantity, for now it'll just calibrate on its own hopefully
                }
                government.askPrices.foreach { (rt, price) =>
                  if (storedResources.getOrElse(rt, 0) >= 10) {
                    context.self ! MakeAsk(regionActor, rt, 10, price)
                  }
                }
              }
              Behaviors.same

            case MakeBid(sendTo, resourceType, quantity, price) =>
              sendTo ! ReceiveBid(context.self, government.id, resourceType, quantity, price)
              Behaviors.same

            case MakeAsk(sendTo, resourceType, quantity, price) =>
              sendTo ! ReceiveAsk(context.self, government.id, resourceType, quantity, price)
              tick(government.copy(
                storedResources = storedResources +
                  (resourceType -> (storedResources.getOrElse(resourceType, 0) - quantity))))

            case PurchaseResource(resourceType, quantity, price) =>
              val updatedResources = storedResources +
                (resourceType -> (storedResources.getOrElse(resourceType, 0) + quantity))
              tick(government.copy(storedResources = updatedResources))

            case ReceiveBond(bond, replyTo, issuedFrom) =>
              val counterOffer = bond.copy(interestRate = government.interestRate)
              replyTo ! Some(counterOffer) // TODO consider risks of having ID mess up matching
              timers.startTimerWithFixedDelay(s"collect-${counterOffer.id}", CollectBondPayment(counterOffer.id, Math.round(counterOffer.principal / 10)), 20.second)
              tick(government.copy(
                econActors = government.econActors + (counterOffer.debtorId -> issuedFrom),
                bonds = government.bonds + (counterOffer.id -> counterOffer)
              ))

            case CollectBondPayment(bondId, amount) =>
              // Look up the current version of the bond
              government.bonds.get(bondId) match {
                case Some(currentBond) =>
                  context.log.info(s"Government collecting payment of $amount on bond ${currentBond.id} from ${currentBond.debtorId}")

                  government.econActors.get(currentBond.debtorId) match {
                    case Some(actorRef) =>
                      context.ask(actorRef, PayBond(currentBond, amount, _)) {
                        case Success(payment: Int) =>
                          val updatedBond = currentBond.copy(totalOutstanding = ((currentBond.totalOutstanding - payment) * currentBond.interestRate).toInt)

                          if (updatedBond.totalOutstanding <= 0) {
                            context.log.info(s"Bond ${currentBond.id} fully repaid by ${currentBond.debtorId}. Final payment: $payment")
                            timers.cancel(s"collect-$bondId")
                          }

                          UpdateBond(updatedBond)
                        case Failure(err) =>
                          context.log.error(s"Failed to collect payment on bond ${currentBond.id}: ${err.getMessage}")
                          ActorNoOp()
                        case _ =>
                          ActorNoOp()
                      }
                    case None =>
                      context.log.warn(s"Could not find actor reference for debtor ${currentBond.debtorId}")
                  }
                case None =>
                  context.log.warn(s"Bond $bondId no longer exists in bonds owned")
                  timers.cancel(s"collect-$bondId")
              }
              Behaviors.same

              // Helper case class to update a bond in the government's state

            // Add this case to the match block in the GovernmentActor's tick method
            case UpdateBond(updatedBond) =>
              if (updatedBond.totalOutstanding <= 0) {
                // Remove the fully paid bond
                tick(government.copy(bonds = government.bonds - updatedBond.id))
              } else {
                // Update the bond with new outstanding amount
                tick(government.copy(bonds = government.bonds + (updatedBond.id -> updatedBond)))
              }

            case AddRegion(uuid, ref) =>
              tick(government.copy(regions = government.regions + (uuid -> ref)))

            case _ =>
              tick(government)
          }
        }
      }

      def initialize(): Behavior[Command] = {
        Behaviors.receive { (context, message) =>
          message match {
            case InitializeGov(government) =>
              timers.startTimerWithFixedDelay("trading-timer", BuyAndSellResources(), 5.seconds)
              tick(government)
            case _ =>
              Behaviors.same
          }
        }
      }

      initialize()
    }
  }
}