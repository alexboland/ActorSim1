import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

case class Government(
                       storedResources: Map[ResourceType, Int],
                       askPrices: Map[ResourceType, Int],
                       bidPrices: Map[ResourceType, Int],
                       regions: Map[String, ActorRef[RegionActor.Command]],
                     )

object Government {
  def newGov() = Government(Map(), Map(Food -> 1), Map(Food -> 1), Map())
}

object GovernmentActor {
  trait Command

  case class InitializeGov(government: Government) extends Command

  case class SetBidPrice(resourceType: ResourceType, price: Int) extends Command

  case class SetAskPrice(resourceType: ResourceType, price: Int) extends Command

  case class InfoResponse(government: Government) extends ManagerActor.Command

  case class AddRegion(uuid: String, ref: ActorRef[RegionActor.Command]) extends Command

  case class BuyResource(resourceType: ResourceType, qty: Int) extends Command // No need for price since govt has unlimited money

  def apply(): Behavior[Command] = Behaviors.setup { context =>

    def tick(government: Government): Behavior[Command] = {
      Behaviors.withTimers { timers =>
        Behaviors.receive { (context, message) =>
          message match {
            case ShowInfo(replyTo) =>
              println("government actor got ShowInfo command")
              replyTo ! InfoResponse(government)
              println("government actor replied to ShowInfo command")
              Behaviors.same

            case BuyResource(resourceType, qty) =>
              val newStoredResources = government.storedResources + (resourceType -> (government.storedResources.getOrElse(resourceType, 0) + qty))
              tick(government.copy(storedResources = newStoredResources))

            case GetBidPrice(replyTo, resourceType) =>
              replyTo ! government.bidPrices.get(resourceType)
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