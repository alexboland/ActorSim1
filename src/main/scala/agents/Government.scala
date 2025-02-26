package agents

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout
import middleware.GameInfo

import scala.concurrent.duration.DurationInt
import java.util.UUID
import scala.util.{Failure, Success}

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

  case class SetBidPrice(resourceType: ResourceType, price: Int) extends GovtCommand

  case class SetAskPrice(resourceType: ResourceType, price: Int) extends GovtCommand

  case class InfoResponse(government: Government) extends GameInfo.InfoResponse {
    val agent: Government = government
  }

  case class AddRegion(uuid: String, ref: ActorRef[RegionActor.Command]) extends GovtCommand


  case class BuyAndSellResources() extends GovtCommand

  type Command = GovtCommand | BankingCommand | GameActorCommand | EconAgent.Command

  implicit val timeout: Timeout = Timeout(3.seconds) // Define an implicit timeout for ask pattern

  def apply(): Behavior[Command] = Behaviors.setup { context =>

    Behaviors.withTimers { timers =>
      def tick(government: Government): Behavior[Command] = {

        Behaviors.receive { (context, message) =>
          message match {
            case ShowInfo(replyTo) =>
              println("government actor got ShowInfo command")
              replyTo ! Some(InfoResponse(government))
              println("government actor replied to ShowInfo command")
              Behaviors.same

            case BuyFromSeller(seller, resourceType, qty, price) =>
              val newStoredResources = government.storedResources + (resourceType -> (government.storedResources.getOrElse(resourceType, 0) + qty))
              seller ! SellToBuyer(context.self, resourceType, qty, price)

              tick(government.copy(storedResources = newStoredResources))

            case SellToBuyer(buyer, resourceType, qty, price)  =>
              val newStoredResources = government.storedResources + (resourceType -> (government.storedResources.getOrElse (resourceType, 0) - qty) )

              tick (government.copy (storedResources = newStoredResources) )

            case GetBidPrice(replyTo, resourceType) =>
              replyTo ! government.bidPrices.get(resourceType)
              Behaviors.same

            case GetAskPrice(replyTo, resourceType) =>
              replyTo ! government.askPrices.get(resourceType)
              Behaviors.same

            case BuyAndSellResources() =>
              government.regions.foreach { (regionUuid, regionActor) =>
                government.bidPrices.foreach { (rt, price) =>
                  context.self ! MakeBid(regionActor, rt, 10, price) // TODO figure out quantity, for now it'll just calibrate on its own hopefully
                }
                government.askPrices.foreach { (rt, price) =>
                  context.self ! MakeAsk(regionActor, rt, 10, price)
                }
              }
              Behaviors.same

            case MakeBid(sendTo, resourceType, quantity, price) =>
              context.ask(sendTo, ReceiveBid(_, resourceType, quantity, price)) {
                case Success(AcceptBid()) =>
                  BuyFromSeller(sendTo, resourceType, quantity, price)
                case Success(RejectBid(None)) =>
                  ActorNoOp()
                case Failure(_) =>
                  ActorNoOp()
                case _ =>
                  ActorNoOp()
              }
              Behaviors.same

            case ReceiveBond(bond, replyTo, issuedFrom) =>
              replyTo ! Some(bond.copy(interestRate = government.interestRate)) // TODO consider risks of having ID mess up matching
              timers.startTimerWithFixedDelay(s"collect-${bond.id}", CollectBondPayment(bond, Math.round(bond.principal / 10)), 20.second)
              tick(government.copy(
                econActors = government.econActors + (bond.debtorId -> issuedFrom),
                bonds = government.bonds + (bond.id -> bond)
              ))

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