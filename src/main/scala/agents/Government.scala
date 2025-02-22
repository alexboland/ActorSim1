package agents

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import middleware.GameInfo
import scala.concurrent.duration.DurationInt

import java.util.UUID

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

  case class BuyResource(resourceType: ResourceType, qty: Int) extends GovtCommand // No need for price since govt has unlimited money
  
  type Command = GovtCommand | BankingCommand | GameActorCommand

  def apply(): Behavior[Command] = Behaviors.setup { context =>

    def tick(government: Government): Behavior[Command] = {
      Behaviors.withTimers { timers =>
        Behaviors.receive { (context, message) =>
          message match {
            case ShowInfo(replyTo) =>
              println("government actor got ShowInfo command")
              replyTo ! Some(InfoResponse(government))
              println("government actor replied to ShowInfo command")
              Behaviors.same

            case BuyResource(resourceType, qty) =>
              val newStoredResources = government.storedResources + (resourceType -> (government.storedResources.getOrElse(resourceType, 0) + qty))
              tick(government.copy(storedResources = newStoredResources))

            case GetBidPrice(replyTo, resourceType) =>
              replyTo ! government.bidPrices.get(resourceType)
              Behaviors.same

            case GetAskPrice(replyTo, resourceType) =>
              replyTo ! government.askPrices.get(resourceType)
              Behaviors.same

            case MakeBid(actor, resourceType, quantity, price) =>
              /*val futureResponse: Future[RegionActor.BidResponse] = actor.ask(replyTo => RegionActor.ReceiveBid(resourceType, quantity, price, replyTo))

              // Handling the future response
              futureResponse.onComplete {
                case Success(response) =>
                  println(s"Received response: ${response.result}")
                case Failure(exception) =>
                  println(s"Failed to receive response: ${exception.getMessage}")
              }*/
              tick(government)

            case ReceiveBond(bond, replyTo, issuedFrom) =>
              replyTo ! Some(bond.copy(interestRate = government.interestRate)) // TODO consider risks of having ID mess up matching
              timers.startTimerWithFixedDelay(s"collect-${bond.id}", CollectBondPayment(bond, Math.round(bond.principal/10)), 20.second)
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
    }

    def initialize(): Behavior[Command] = {
      Behaviors.receive { (context, message) =>
        message match {
          case InitializeGov(government) =>
            tick(government)
          case _ =>
            Behaviors.same
        }
      }
    }

    initialize()
  }
}