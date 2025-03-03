package agents

import agents.EconAgent.CounterOffer
import agents.Market.*
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout
import middleware.GameInfo

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

case class MarketActorState(
                           market: Market,
                           regionActor: ActorRef[RegionActor.Command]
)

case class Market(
                 id: String,
                 regionId: String,
                 localId: String,
                 storedResources: Map[ResourceType, Int],
                 buyPrices: Map[ResourceType, Int],
                 sellPrices: Map[ResourceType, Int],
                 outstandingBonds: Map[String, Bond]
               ) extends EconAgent

object Market {

  trait Command

  case class SetBuyPrice(resourceType: ResourceType, price: Int) extends Command
  case class SetSellPrice(resourceType: ResourceType, price: Int) extends Command
  case class GetSellPrice(replyTo: ActorRef[Option[Int]], resourceType: ResourceType) extends Command

  def newMarket(regionId: String): Market = {
    Market(
      id = UUID.randomUUID().toString,
      regionId = regionId,
      localId = "market",
      storedResources = Map(),
      buyPrices = Map(),
      sellPrices = Map(),
      outstandingBonds = Map()
    )
  }
}

object MarketActor {

  case class InfoResponse(market: Market) extends GameInfo.InfoResponse {
    override val agent: Market = market
  }

  type Command = Market.Command | GameActorCommand | EconAgent.Command

  implicit val timeout: Timeout = Timeout(3.seconds) // Define an implicit timeout for ask pattern

  def apply(state: MarketActorState): Behavior[Command] = Behaviors.setup { context =>
    def tick(state: MarketActorState): Behavior[Command] = {
      val market = state.market
      Behaviors.receive { (context, message) =>
        message match {

          case SetBuyPrice(resourceType, price) =>
            val newPrices = market.buyPrices + (resourceType -> price)
            tick(state.copy(market = market.copy(buyPrices = newPrices)))

          case SetSellPrice(resourceType, price) =>
            val newPrices = market.sellPrices + (resourceType -> price)
            tick(state.copy(market = market.copy(sellPrices = newPrices)))

          case GetSellPrice(replyTo, resourceType) =>
            replyTo ! market.sellPrices.get(resourceType)
            Behaviors.same

          case MakeBid(sendTo, resourceType, quantity, price) =>
            context.ask(sendTo, ReceiveBid(_, resourceType, quantity, price)) {
              case Success(AcceptBid()) =>
                BuyFromSeller(sendTo, resourceType, quantity, price)
              case Success(RejectBid(None)) =>
                ActorNoOp()
              case Failure(_) =>
                ActorNoOp()
              case _ =>
                ActorNoOp()

            }
            Behaviors.same

          case BuyFromSeller(seller, resourceType, quantity, price) =>
            seller ! SellToBuyer(context.self, resourceType, quantity, price)
            val updatedResources = market.storedResources +
              (resourceType -> (market.storedResources.getOrElse(resourceType, 0) + quantity),
                Money -> (market.storedResources.getOrElse(Money, 0) - Math.multiplyExact(quantity, price)))

            tick(state.copy(market = market.copy(storedResources = updatedResources)))

          case SellToBuyer(buyer, resourceType, quantity, price) =>
            //Note that currently this is supposed to never be the "initiating" transaction so assume other actor already ran the "buy" command
            val updatedResources = market.storedResources +
              (resourceType -> (market.storedResources.getOrElse(resourceType, 0) - quantity),
                Money -> (market.storedResources.getOrElse(Money, 0) + Math.multiplyExact(quantity, price)))

            tick(state.copy(market = market.copy(storedResources = updatedResources)))


          case ReceiveBid(replyTo, resourceType, quantity, price) =>
            // If it either doesn't have enough to sell or the price is too low, reject
            val available = market.storedResources.getOrElse(resourceType, 0)
            if (available < quantity || price < market.sellPrices.getOrElse(resourceType, 0)) {
              if (available <= 0 || price <= 0) {
                replyTo ! RejectBid(None)
              } else {
                val coPrice = Math.max(price, market.sellPrices.getOrElse(resourceType, 0))
                val coQty = Math.min(quantity, market.storedResources.getOrElse(resourceType, 0))
                replyTo ! RejectBid(Some(CounterOffer(coQty, coPrice)))
              }
              tick(state) // Not selling, do nothing yet
            } else {
              replyTo ! AcceptBid()
              val updatedResources = market.storedResources +
                (resourceType -> (market.storedResources.getOrElse(resourceType, 0) - quantity)) +
                (Money -> (market.storedResources.getOrElse(Money, 0) + quantity*price))
              tick(state.copy(
                market = market.copy(storedResources = updatedResources)))
            }

          case ReceiveAsk(replyTo, resourceType, quantity, price) =>
            val marketPrice = market.buyPrices.getOrElse(resourceType, 0)
            val funds = market.storedResources.getOrElse(Money, 0)
            if (marketPrice < price || funds < price*quantity) {
              if (funds < price*quantity) {
                context.self ! IssueBond(state.regionActor, price*quantity, 0.05) // temporary heuristics
              }
              replyTo ! RejectAsk(Some(CounterOffer(Math.min(funds, quantity*price), Math.min(marketPrice, price))))
              tick(state)
            } else {
              replyTo ! AcceptAsk()
              val updatedResources = market.storedResources +
                (resourceType -> (market.storedResources.getOrElse(resourceType, 0) + quantity)) +
                (Money -> (market.storedResources.getOrElse(Money, 0) - quantity*price))
              tick(state.copy(
                market = market.copy(storedResources = updatedResources)))

            }

          case IssueBond(sendTo, principal, interest) =>
            val bond = Bond(UUID.randomUUID().toString, principal, interest, principal, market.id)
            context.ask(sendTo, ReceiveBond(bond, _, context.self)) {
              case Success(Some(offered: Bond)) =>
                if (bond == offered) {
                  AddOutstandingBond(bond)
                } else {
                  IssueBond(sendTo, offered.principal, offered.interestRate) // Repeat but with their counteroffer
                }
              case _ =>
                ActorNoOp()
            }
            Behaviors.same

          case AddOutstandingBond(bond) =>
            val updatedResources = market.storedResources + (Money -> (market.storedResources.getOrElse(Money, 0) + bond.principal))

            val newOutstandingBonds = market.outstandingBonds + (bond.id -> bond)

            tick(state = state.copy(market = market.copy(storedResources = updatedResources, outstandingBonds = newOutstandingBonds)))

          case PayBond(bond, amount, replyTo) =>
            val amountToPay = Math.min(market.storedResources.getOrElse(Money, 0), amount) // For now just have it pay what it can without defaults
            val updatedBond = bond.copy(totalOutstanding = Math.round((bond.totalOutstanding - amountToPay)*bond.interestRate).toInt)
            val newOutstandingBonds = if (updatedBond.totalOutstanding <= 0) {
              market.outstandingBonds - bond.id
            } else {
              market.outstandingBonds + (bond.id -> updatedBond)
            }
            val updatedResources = market.storedResources + (Money -> (market.storedResources.getOrElse(Money, 0) - amountToPay))
            replyTo ! amountToPay
            tick(state = state.copy(market = market.copy(storedResources = updatedResources, outstandingBonds = newOutstandingBonds)))

          case _ =>
            Behaviors.same
        }
      }
    }



    Behaviors.withTimers { timers =>
      //timers.startTimerWithFixedDelay("production", ProduceResource(), 8.second)

      tick(state)
    }
  }
}