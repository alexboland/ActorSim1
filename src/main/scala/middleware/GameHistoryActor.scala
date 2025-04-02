package agents

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import middleware.{EventType, GameEvent, GameHistory}

object GameHistoryActor {
  // Command trait and message definitions
  sealed trait Command

  // Add a new event to the history
  case class AddGameEvent(event: GameEvent) extends Command

  // Query history with optional filters
  case class GetGameHistory(
                             agentId: Option[String] = None,
                             regionId: Option[String] = None,
                             eventType: Option[EventType] = None,
                             fromTime: Option[Long] = None,
                             toTime: Option[Long] = None,
                             offset: Int = 0,
                             limit: Int = 100,
                             replyTo: ActorRef[GameHistoryResponse]
                           ) extends Command

  // Response message for history queries
  case class GameHistoryResponse(events: Vector[GameEvent], totalCount: Int)

  // Helper method to create events easily
  def createEvent(
                   agentId: String,
                   regionId: String,
                   eventType: EventType,
                   eventText: String
                 ): GameEvent = {
    GameEvent(
      timestamp = System.currentTimeMillis(),
      agentId = agentId,
      regionId = regionId,
      eventType = eventType,
      eventText = eventText
    )
  }

  def apply(): Behavior[Command] = behavior(GameHistory())

  private def behavior(gameHistory: GameHistory): Behavior[Command] =
    Behaviors.receive { (context, message) =>
      message match {
        case AddGameEvent(event) =>
          // Add the event to our history
          val updatedHistory = gameHistory.addEvent(event)
          // Optionally log the event to console for debugging
          //context.log.debug(s"Game event: $event")
          behavior(updatedHistory)

        case GetGameHistory(agentId, regionId, eventType, fromTime, toTime, offset, limit, replyTo) =>
          val (results, totalCount) = gameHistory.search(agentId, regionId, eventType, fromTime, toTime, offset, limit)
          replyTo ! GameHistoryResponse(results, totalCount)
          Behaviors.same
      }
    }
}