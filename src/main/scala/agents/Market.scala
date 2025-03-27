package agents

import agents.EconAgent.CounterOffer
import agents.Market.*
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import middleware.{EventType, GameEvent, GameEventService, GameInfo}

import java.util.UUID
import scala.concurrent.duration.DurationInt

case class MarketActorState(
                             market: Market,
                             regionActor: ActorRef[RegionActor.Command],
                             econActorIds: Map[String, EconActor]
                           )

case class Market(
                   id: String,
                   regionId: String,
                   localId: String,
                   bids: Map[ResourceType, List[Bid]] = Map.empty.withDefaultValue(List.empty),
                   asks: Map[ResourceType, List[Ask]] = Map.empty.withDefaultValue(List.empty)
                 ) extends EconAgent

object Market {
  trait Command

  case class Bid(
                  buyerId: String,
                  resourceType: ResourceType,
                  quantity: Int,
                  price: Int
                )

  case class Ask(
                  sellerId: String,
                  resourceType: ResourceType,
                  quantity: Int,
                  price: Int
                )

  case class MarketTransaction(
                                resourceType: ResourceType,
                                quantity: Int,
                                price: Int
                              )

  def newMarket(regionId: String): Market = {
    Market(
      id = UUID.randomUUID().toString,
      regionId = regionId,
      localId = "market"
    )
  }
}

object MarketActor {
  case class InfoResponse(market: Market) extends GameInfo.InfoResponse {
    override val agent: Market = market
  }

  type Command = Market.Command | GameActorCommand | EconAgent.Command

  def apply(state: MarketActorState): Behavior[Command] = Behaviors.setup { context =>
    // Create event service
    val eventService = GameEventService(context)

    def tick(state: MarketActorState): Behavior[Command] = {
      val market = state.market

      // Helper function for logging events
      def logMarketEvent(eventType: EventType, eventText: String): Unit = {
        eventService.logEvent(
          agentId = market.id,
          regionId = market.regionId,
          eventType = eventType,
          eventText = eventText
        )
      }

      Behaviors.receive { (context, message) =>
        message match {
          case ShowInfo(replyTo) =>
            replyTo ! Some(InfoResponse(state.market))
            Behaviors.same

          case GetBidPrice(replyTo, resourceType) =>
            val highestBid = state.market.bids.get(resourceType).flatMap(_.headOption)
            replyTo ! highestBid.map(_.price)
            Behaviors.same

          case GetAskPrice(replyTo, resourceType) =>
            val lowestAsk = state.market.asks.get(resourceType).flatMap(_.headOption)
            replyTo ! lowestAsk.map(_.price)
            Behaviors.same

          case ReceiveBid(replyTo, bidderId, resourceType, quantity, price) =>
            // Store the bidder's actor reference in econActorIds
            val updatedEconActorIds = state.econActorIds + (bidderId -> replyTo)
            val newState = state.copy(econActorIds = updatedEconActorIds)

            val newBid = Bid(bidderId, resourceType, quantity, price)
            logMarketEvent(
              EventType.Custom("BidReceived"),
              s"Bid received from $bidderId for $quantity $resourceType at price $price"
            )

            // Look for matching asks (lowest price first that meets criteria)
            val matchingAsks = market.asks.getOrElse(resourceType, List.empty)
              .filter(ask => ask.price <= price)
            // Already sorted by lowest first, as per storage

            if (matchingAsks.isEmpty) {
              // Bid is unfulfilled, send out signal to region (founders)
              newState.regionActor ! ReceiveBid(context.self, market.id, resourceType, quantity, price)
              // No matching asks, add bid to queue
              val currentBids = market.bids.getOrElse(resourceType, List.empty)
              // Insert maintaining sort by highest price first
              val updatedBidsList = (newBid :: currentBids).sortBy(-_.price)
              val updatedBids = market.bids + (resourceType -> updatedBidsList)
              val updatedMarket = market.copy(bids = updatedBids)
              tick(newState.copy(market = updatedMarket))
            } else {
              // Found a matching ask
              val bestAsk = matchingAsks.head
              val tradeQuantity = Math.min(quantity, bestAsk.quantity)

              // Look up seller's ActorRef using sellerId in econActorIds
              val sellerActor = newState.econActorIds.get(bestAsk.sellerId)

              // Send messages to both parties to adjust their funds
              sellerActor.foreach(_ ! ReceiveSalePayment(tradeQuantity * bestAsk.price))
              replyTo ! PurchaseResource(resourceType, tradeQuantity, bestAsk.price)

              // Log transaction
              logMarketEvent(
                EventType.MarketTransaction,
                s"Transaction: $tradeQuantity $resourceType sold by ${bestAsk.sellerId} to $bidderId at price ${bestAsk.price}"
              )

              // Update asks list
              val currentAsks = newState.market.asks.getOrElse(resourceType, List.empty)
              val updatedAsks = if (bestAsk.quantity > tradeQuantity) {
                // Partial fill, keep ask with reduced quantity (no need to send any message)
                val updatedAsk = bestAsk.copy(quantity = bestAsk.quantity - tradeQuantity)
                val newAsksList = (updatedAsk :: currentAsks.filterNot(_ == bestAsk)).sortBy(_.price)
                newState.market.asks + (resourceType -> newAsksList)
              } else {
                // Full fill, remove ask
                val newAsksList = currentAsks.filterNot(_ == bestAsk)
                if (newAsksList.isEmpty) newState.market.asks - resourceType
                else newState.market.asks + (resourceType -> newAsksList)
              }

              val updatedMarket = newState.market.copy(asks = updatedAsks)

              // If we didn't fulfill the entire bid, add remainder as a new bid (this way it will look for more asks rather than staying in limbo)
              if (tradeQuantity < quantity) {
                // Recursively process the remaining quantity by sending a new bid to self
                context.self ! ReceiveBid(replyTo, bidderId, resourceType, quantity - tradeQuantity, price)
              }

              tick(newState.copy(market = updatedMarket))
            }

          case ReceiveAsk(replyTo, sellerId, resourceType, quantity, price) =>
            // Store the seller's actor reference in econActorIds
            val updatedEconActorIds = state.econActorIds + (sellerId -> replyTo)
            val newState = state.copy(econActorIds = updatedEconActorIds)

            val newAsk = Ask(sellerId, resourceType, quantity, price)
            logMarketEvent(
              EventType.Custom("AskReceived"),
              s"Ask received from $sellerId for $quantity $resourceType at price $price"
            )

            // Look for matching bids (highest price first that meets criteria)
            val matchingBids = newState.market.bids.getOrElse(resourceType, List.empty)
              .filter(bid => bid.price >= price)
            // Already sorted by highest first, as per storage

            if (matchingBids.isEmpty) {
              // No matching bids, add ask to queue
              val currentAsks = newState.market.asks.getOrElse(resourceType, List.empty)
              // Insert maintaining sort by lowest price first
              val updatedAsksList = (newAsk :: currentAsks).sortBy(_.price)
              val updatedAsks = newState.market.asks + (resourceType -> updatedAsksList)
              val updatedMarket = newState.market.copy(asks = updatedAsks)
              tick(newState.copy(market = updatedMarket))
            } else {
              // Found a matching bid
              val bestBid = matchingBids.head
              val tradeQuantity = Math.min(quantity, bestBid.quantity)

              // Look up buyer's ActorRef using buyerId in econActorIds
              val buyerActor = newState.econActorIds.get(bestBid.buyerId)

              // Notify both parties
              buyerActor.foreach(_ ! PurchaseResource(resourceType, tradeQuantity, price))
              replyTo ! ReceiveSalePayment(tradeQuantity * price)

              // Log transaction
              logMarketEvent(
                EventType.MarketTransaction,
                s"Transaction: $tradeQuantity $resourceType sold by $sellerId to ${bestBid.buyerId} at price $price"
              )

              // Update bids list
              val currentBids = newState.market.bids.getOrElse(resourceType, List.empty)
              val updatedBids = if (bestBid.quantity > tradeQuantity) {
                // Partial fill, keep bid with reduced quantity
                val updatedBid = bestBid.copy(quantity = bestBid.quantity - tradeQuantity)
                val newBidsList = (updatedBid :: currentBids.filterNot(_ == bestBid)).sortBy(-_.price)
                newState.market.bids + (resourceType -> newBidsList)
              } else {
                // Full fill, remove bid
                val newBidsList = currentBids.filterNot(_ == bestBid)
                if (newBidsList.isEmpty) newState.market.bids - resourceType
                else newState.market.bids + (resourceType -> newBidsList)
              }

              val updatedMarket = newState.market.copy(bids = updatedBids)

              // If we didn't fulfill the entire ask, add remainder as a new ask or process more bids
              if (tradeQuantity < quantity) {
                // Recursively process the remaining quantity by sending a new ask to self
                context.self ! ReceiveAsk(replyTo, sellerId, resourceType, quantity - tradeQuantity, price)
              }

              tick(newState.copy(market = updatedMarket))
            }

          case _ =>
            Behaviors.same
        }
      }
    }

    Behaviors.withTimers { timers =>
      // Could add periodic cleanup of stale bids/asks if needed
      tick(state)
    }
  }
}