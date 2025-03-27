package middleware

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.actor.typed.scaladsl.ActorContext
import agents.ManagerActor

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
 * Default implementation that uses the top-level ManagerActor
 */
class SystemGuardianEventService(system: ActorSystem[ManagerActor.Command]) extends GameEventService {
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
    system ! ManagerActor.AddGameEvent(event)
  }
}

/**
 * Implementation that uses an explicit reference to the ManagerActor
 */
class DirectEventService(managerActor: ActorRef[ManagerActor.Command]) extends GameEventService {
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
    managerActor ! ManagerActor.AddGameEvent(event)
  }
}

/**
 * Factory object for creating GameEventService instances
 */
object GameEventService {
  /**
   * Create a service instance using the actor system
   */
  def apply(system: ActorSystem[_]): GameEventService = {
    new SystemGuardianEventService(system.asInstanceOf[ActorSystem[ManagerActor.Command]])
  }

  /**
   * Create a service instance using a direct reference to the ManagerActor
   */
  def apply(managerActor: ActorRef[ManagerActor.Command]): GameEventService = {
    new DirectEventService(managerActor)
  }

  /**
   * Create a service instance from an actor context
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