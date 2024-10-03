import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object PingableActor {
  sealed trait Command
  case class Ping(replyTo: ActorRef[Pong.type]) extends Command
  case object Pong

  def apply(): Behavior[Command] = Behaviors.receiveMessage {
    case Ping(replyTo) =>
      println("Ping received")
      replyTo ! Pong
      Behaviors.same
  }
}
