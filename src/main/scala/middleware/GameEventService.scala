package middleware

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import agents.{GameHistoryActor, ManagerActor}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Await}

/**
 * Simplified service trait for game event logging with a single core method
 */
trait GameEventService {
  /**
   * Core method - log a game event with all parameters specified
   */
  def logEvent(
                agentId: String,
                regionId: String,
                eventType: EventType,
                eventText: String
              ): Unit

  /**
   * Alternative to directly create and log a GameEvent object
   */
  def logEvent(event: GameEvent): Unit = {
    logEvent(event.agentId, event.regionId, event.eventType, event.eventText)
  }
}

/**
 * Implementation that uses a direct reference to the GameHistoryActor
 */
class DirectEventService(gameHistoryActor: ActorRef[GameHistoryActor.Command]) extends GameEventService {
  override def logEvent(
                         agentId: String,
                         regionId: String,
                         eventType: EventType,
                         eventText: String
                       ): Unit = {
    val event = GameEvent(
      timestamp = System.currentTimeMillis(),
      agentId = agentId,
      regionId = regionId,
      eventType = eventType,
      eventText = eventText
    )
    gameHistoryActor ! GameHistoryActor.AddGameEvent(event)
  }
}

/**
 * Factory object for creating GameEventService instances
 */
object GameEventService {
  /**
   * Create a service instance using the actor system
   * This gets the GameHistoryActor reference from ManagerActor
   */
  def apply(system: ActorSystem[_]): GameEventService = {
    implicit val timeout: Timeout = 3.seconds
    implicit val ec: ExecutionContext = system.executionContext
    implicit val scheduler = system.scheduler

    // Get GameHistoryActor reference from the ManagerActor
    val managerSystem = system.asInstanceOf[ActorSystem[ManagerActor.Command]]
    val future = managerSystem.ask[ActorRef[GameHistoryActor.Command]](ManagerActor.GetGameHistoryActor)
    val gameHistoryActor = Await.result(future, timeout.duration)

    // Create a DirectEventService with the GameHistoryActor
    new DirectEventService(gameHistoryActor)
  }

  /**
   * Create a service instance using a direct reference to the GameHistoryActor
   */
  def apply(gameHistoryActor: ActorRef[GameHistoryActor.Command]): GameEventService = {
    new DirectEventService(gameHistoryActor)
  }

  /**
   * Create a service instance from an actor context
   * This will use the system to get the GameHistoryActor reference
   */
  def apply[T](context: ActorContext[T]): GameEventService = {
    apply(context.system)
  }

  /**
   * Get a no-op implementation that doesn't log anything (useful for testing)
   */
  def noOp: GameEventService = NoOpEventService

  /**
   * Implementation that does nothing (for testing)
   */
  private object NoOpEventService extends GameEventService {
    override def logEvent(
                           agentId: String,
                           regionId: String,
                           eventType: EventType,
                           eventText: String
                         ): Unit = {}
  }
}