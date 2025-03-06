package agents

import agents.EconAgent.CounterOffer
import agents.Market.*
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import middleware.GameInfo

import java.util.UUID
import scala.concurrent.duration.DurationInt

case class MarketActorState(
                             market: Market,
                             regionActor: ActorRef[RegionActor.Command]
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

  case class GetHighestBid(resourceType: ResourceType, replyTo: ActorRef[Option[Bid]]) extends Command
  case class GetLowestAsk(resourceType: ResourceType, replyTo: ActorRef[Option[Ask]]) extends Command

  case class Bid(
                  buyer: ActorRef[EconAgent.Command],
                  resourceType: ResourceType,
                  quantity: Int,
                  price: Int
                )

  case class Ask(
                  seller: ActorRef[EconAgent.Command],
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
    def tick(state: MarketActorState): Behavior[Command] = {
      Behaviors.receive { (context, message) =>
        message match {
          case ShowInfo(replyTo) =>
            replyTo ! Some(InfoResponse(state.market))
            Behaviors.same

          case GetHighestBid(resourceType, replyTo) =>
            val highestBid = state.market.bids.get(resourceType).flatMap(_.headOption)
            replyTo ! highestBid
            Behaviors.same

          case GetLowestAsk(resourceType, replyTo) =>
            val lowestAsk = state.market.asks.get(resourceType).flatMap(_.headOption)
            replyTo ! lowestAsk
            Behaviors.same

          case ReceiveBid(replyTo, resourceType, quantity, price) =>
            val newBid = Bid(replyTo, resourceType, quantity, price)

            // Look for matching asks (lowest price first that meets criteria)
            val matchingAsks = state.market.asks.getOrElse(resourceType, List.empty)
              .filter(ask => ask.price <= price)
            // Already sorted by lowest first, as per storage

            if (matchingAsks.isEmpty) {
              // Bid is unfulfilled, send out signal to region (founders)
              state.regionActor ! ReceiveBid(context.self, resourceType, quantity, price)
              // No matching asks, add bid to queue
              val currentBids = state.market.bids.getOrElse(resourceType, List.empty)
              // Insert maintaining sort by highest price first
              val updatedBidsList = (newBid :: currentBids).sortBy(-_.price)
              val updatedBids = state.market.bids + (resourceType -> updatedBidsList)
              val updatedMarket = state.market.copy(bids = updatedBids)
              tick(state.copy(market = updatedMarket))
            } else {
              // Found a matching ask
              val bestAsk = matchingAsks.head
              val tradeQuantity = Math.min(quantity, bestAsk.quantity)

              // Send messages to both parties to adjust their funds
              bestAsk.seller ! ReceiveSalePayment(tradeQuantity * bestAsk.price)
              replyTo ! PurchaseResource(resourceType, tradeQuantity, bestAsk.price)

              // Update asks list
              val currentAsks = state.market.asks.getOrElse(resourceType, List.empty)
              val updatedAsks = if (bestAsk.quantity > tradeQuantity) {
                // Partial fill, keep ask with reduced quantity (no need to send any message)
                val updatedAsk = bestAsk.copy(quantity = bestAsk.quantity - tradeQuantity)
                val newAsksList = (updatedAsk :: currentAsks.filterNot(_ == bestAsk)).sortBy(_.price)
                state.market.asks + (resourceType -> newAsksList)
              } else {
                // Full fill, remove ask
                val newAsksList = currentAsks.filterNot(_ == bestAsk)
                if (newAsksList.isEmpty) state.market.asks - resourceType
                else state.market.asks + (resourceType -> newAsksList)
              }

              val updatedMarket = state.market.copy(asks = updatedAsks)

              // If we didn't fulfill the entire bid, add remainder as a new bid (this way it will look for more asks rather than staying in limbo)
              if (tradeQuantity < quantity) {
                // Recursively process the remaining quantity by sending a new bid to self
                context.self ! ReceiveBid(replyTo, resourceType, quantity - tradeQuantity, price)
              }

              tick(state.copy(market = updatedMarket))
            }

          case ReceiveAsk(replyTo, resourceType, quantity, price) =>
            val newAsk = Ask(replyTo, resourceType, quantity, price)

            // Look for matching bids (highest price first that meets criteria)
            val matchingBids = state.market.bids.getOrElse(resourceType, List.empty)
              .filter(bid => bid.price >= price)
            // Already sorted by highest first, as per storage

            if (matchingBids.isEmpty) {
              // No matching bids, add ask to queue
              val currentAsks = state.market.asks.getOrElse(resourceType, List.empty)
              // Insert maintaining sort by lowest price first
              val updatedAsksList = (newAsk :: currentAsks).sortBy(_.price)
              val updatedAsks = state.market.asks + (resourceType -> updatedAsksList)
              val updatedMarket = state.market.copy(asks = updatedAsks)
              tick(state.copy(market = updatedMarket))
            } else {
              // Found a matching bid
              val bestBid = matchingBids.head
              val tradeQuantity = Math.min(quantity, bestBid.quantity)

              // Notify both parties
              bestBid.buyer ! PurchaseResource(resourceType, tradeQuantity, price)
              replyTo ! ReceiveSalePayment(tradeQuantity * price)

              // Update bids list
              val currentBids = state.market.bids.getOrElse(resourceType, List.empty)
              val updatedBids = if (bestBid.quantity > tradeQuantity) {
                // Partial fill, keep bid with reduced quantity
                val updatedBid = bestBid.copy(quantity = bestBid.quantity - tradeQuantity)
                val newBidsList = (updatedBid :: currentBids.filterNot(_ == bestBid)).sortBy(-_.price)
                state.market.bids + (resourceType -> newBidsList)
              } else {
                // Full fill, remove bid
                val newBidsList = currentBids.filterNot(_ == bestBid)
                if (newBidsList.isEmpty) state.market.bids - resourceType
                else state.market.bids + (resourceType -> newBidsList)
              }

              val updatedMarket = state.market.copy(bids = updatedBids)

              // If we didn't fulfill the entire ask, add remainder as a new ask or process more bids
              if (tradeQuantity < quantity) {
                // Recursively process the remaining quantity by sending a new ask to self
                context.self ! ReceiveAsk(replyTo, resourceType, quantity - tradeQuantity, price)
              }

              tick(state.copy(market = updatedMarket))
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