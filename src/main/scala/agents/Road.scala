package agents

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

case class Road(id: String, origin: EconActor, destination: EconActor) extends GameAgent // TODO change this to point to IDs rather than the actor refs

object RoadActor {
  trait Command extends GameActorCommand

  // Create a delay based on the quality of the road
  // Bids and payments could be seen as "instantaneous" due to less logistical issues, maybe...
  case class TransitResource(sender: EconActor, recipient: EconActor, resourceType: ResourceType, quantity: Int) extends Command

  def apply(road: Road): Behavior[Command] = Behaviors.setup { context =>
      Behaviors.receive { (context, message) =>
        message match {
          case _ =>
            Behaviors.same
        }
      }
  }
}