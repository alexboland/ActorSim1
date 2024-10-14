import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._

case class Road(origin: ActorRef[EconActorCommand], destination: ActorRef[EconActorCommand]) extends GameAgent

object RoadActor {
  trait Command extends GameActorCommand

  // Create a delay based on the quality of the road
  // Bids and payments could be seen as "instantaneous" due to less logistical issues, maybe...
  case class TransitResource(sender: ActorRef[EconActorCommand], recipient: ActorRef[EconActorCommand], resourceType: ResourceType, quantity: Int) extends Command

  def apply(road: Road): Behavior[Command] = Behaviors.setup { context =>
      Behaviors.receive { (context, message) =>
        message match {
          case _ =>
            Behaviors.same
        }
      }
  }
}