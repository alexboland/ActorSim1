package agents

import agents.EconAgent.CounterOffer
import agents.Producer.*
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout
import middleware.{EventType, GameEvent, GameEventService, GameInfo}

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

case class ProducerActorState(
                           producer: Producer,
                           regionActor: ActorRef[RegionActor.Command],
                           marketActor: ActorRef[MarketActor.Command],
                           bankActor: ActorRef[BankActor.Command],
                           bidPrices: Map[ResourceType, Int]
                             )

case class Producer(
                 id: String,
                 regionId: String,
                 workers: Int,
                 resourceProduced: ResourceType,
                 storedResources: Map[ResourceType, Int],
                 wage: Int,
                 maxWorkers: Int,
                 baseProduction: Int,
                 inputs: Map[ResourceType, Int],
                 multipliers: Map[ResourceType, Int],
                 outstandingBonds: Map[String, Bond]
               ) extends EconAgent

object Producer {
  def newProducer(regionId: String, baseProduction: Int, resourceProduced: ResourceType): Producer = {
    Producer(
      id = UUID.randomUUID().toString,
      regionId = regionId,
      workers = 0,
      resourceProduced = resourceProduced,
      storedResources = Map[ResourceType, Int](),
      wage = 0,
      maxWorkers = 10,
      baseProduction = baseProduction,
      inputs = Map(),
      multipliers = Map(),
      outstandingBonds = Map())
  }

  trait Command

  case class HireWorkers(wage: Int) extends Command

  case class SetWage(wage: Int) extends Command

  case class ProcureResource(resourceType: ResourceType, qty: Int) extends Command

  case class SellResource(resourceType: ResourceType, qty: Int) extends Command

  case class SetAskPrice(price: Int) extends Command

  case class SetBidPrice(resourceType: ResourceType, price: Int) extends Command

}

object ProducerActor {

  case class InfoResponse(producer: Producer) extends GameInfo.InfoResponse {
    override val agent: Producer = producer
  }

  // TODO copy instances of ResourceProducer.Command to Producer.Command as refactor
  type Command = ResourceProducer.Command | GameActorCommand | Producer.Command | EconAgent.Command

  implicit val timeout: Timeout = Timeout(3.seconds) // Define an implicit timeout for ask pattern

  def apply(state: ProducerActorState): Behavior[Command] = Behaviors.setup { context =>
    // Create event service
    val eventService = GameEventService(context)

    def tick(state: ProducerActorState): Behavior[Command] = {
      val producer = state.producer
      val resourceProduced = producer.resourceProduced
      val storedResources = producer.storedResources

      // Helper function for logging events
      def logProducerEvent(eventType: EventType, eventText: String): Unit = {
        eventService.logEvent(
          agentId = producer.id,
          regionId = producer.regionId,
          eventType = eventType,
          eventText = eventText
        )
      }

      Behaviors.receive { (context, message) =>
        message match {

          case PayWages() =>
            val workersToPay = Math.min(Math.max(Math.floorDiv(storedResources.getOrElse(Money, 0), producer.wage), 0), producer.workers)

            if (workersToPay < producer.maxWorkers) {
              logProducerEvent(
                EventType.Custom("InsufficientFunds"),
                s"Not enough money to pay all workers. Issuing bond to cover wages."
              )
              context.self ! IssueBond(state.bankActor, producer.maxWorkers * producer.wage, 0.01)
            }

            val updatedResources = storedResources +
              (Money -> (storedResources.getOrElse(Money, 0) - (workersToPay * producer.wage)))

            logProducerEvent(
              EventType.Custom("WagesPaid"),
              s"Paid wages to $workersToPay workers at ${producer.wage} per worker."
            )

            // TODO need to send a message to the region to release the workers
            tick(state.copy(producer = producer.copy(
              workers = workersToPay, storedResources = updatedResources)))

          case ProduceResource() =>
            // TODO have total production take into account multipliers

            val maxProduction = Math.multiplyExact(producer.baseProduction, producer.workers)

            val requiredInputs = producer.inputs.view.mapValues(_ * maxProduction)

            val possibleOutput = producer.inputs.foldLeft(0) {
              case (max, (r, qty)) =>
                val limit = storedResources.getOrElse(r, 0) / qty
                if (limit < maxProduction*2) {
                  // Side effect: send a message to self to order enough units for the next order plus the amount missing
                  context.self ! ProcureResource(r, (maxProduction*2) - qty)
                }
                Math.max(limit, max)
            }

            val totalProduction = Math.max(maxProduction, possibleOutput)

            if (totalProduction > 0) {
              logProducerEvent(
                EventType.ResourceProduced,
                s"Produced $totalProduction units of $resourceProduced"
              )
            } else {
              logProducerEvent(
                EventType.Custom("ProductionFailed"),
                s"Failed to produce any $resourceProduced. Not enough resources or workers."
              )
            }

            val updatedResources = producer.inputs.foldLeft(storedResources) {
              case (acc, (r, qty)) =>
                val consumed = producer.inputs.getOrElse(r, 0) * totalProduction
                if (consumed > 0) {
                  logProducerEvent(
                    EventType.ResourceConsumed,
                    s"Consumed $consumed units of $r for production"
                  )
                }
                val newQty = storedResources.getOrElse(r, 0) - consumed
                storedResources + (r -> newQty)
            } + (resourceProduced -> (storedResources.getOrElse(resourceProduced, 0) + totalProduction))

            // For now, have it sell the resources at this point, the message will go to a queue
            if (totalProduction > 0) {
              context.self ! SellResource(producer.resourceProduced, totalProduction)
            }

            tick(state.copy(
              producer = producer.copy(storedResources = updatedResources)))

          case ProcureResource(resourceType, quantity) =>
            logProducerEvent(
              EventType.Custom("ResourceProcurement"),
              s"Attempting to procure $quantity units of $resourceType"
            )

            context.ask(state.marketActor, GetAskPrice(_, resourceType)) {
              case Success(Some(price: Int)) =>
                MakeBid(state.marketActor, resourceType, quantity, price)
              case Success(None) =>
                //Nothing to be done, next attempt at production will come with another attempt at procurement
                logProducerEvent(
                  EventType.Custom("ProcurementFailed"),
                  s"Failed to find ask price for $resourceType"
                )
                ActorNoOp()
              case _ =>
                ActorNoOp()
            }
            Behaviors.same

          case SellResource(resourceType, quantity) =>
            logProducerEvent(
              EventType.Custom("ResourceSale"),
              s"Attempting to sell $quantity units of $resourceType"
            )

            context.ask(state.marketActor, GetBidPrice(_, resourceType)) {
              case Success(Some(price: Int)) =>
                val askPrice = Math.max(price, calculateProductionCost(state))
                MakeAsk(state.marketActor, resourceType, quantity, askPrice)
              case Success(None) =>
                val costPrice = calculateProductionCost(state)
                logProducerEvent(
                  EventType.Custom("NoBids"),
                  s"No bids found for $resourceType. Using cost price of $costPrice."
                )
                MakeAsk(state.marketActor, resourceType, quantity, costPrice)
              case _ =>
                ActorNoOp()
            }
            Behaviors.same

          case MakeBid(sendTo, resourceType, quantity, price) =>
            logProducerEvent(
              EventType.Custom("BidPlaced"),
              s"Placed bid for $quantity units of $resourceType at price $price"
            )

            sendTo ! ReceiveBid(context.self, producer.id, resourceType, quantity, price)
            val updatedBidPrices = state.bidPrices + (resourceType -> price)
            tick(state = state.copy(bidPrices = updatedBidPrices))

          case MakeAsk(sendTo, resourceType, quantity, price) =>
            logProducerEvent(
              EventType.Custom("AskPlaced"),
              s"Placed ask for $quantity units of $resourceType at price $price"
            )

            sendTo ! ReceiveAsk(context.self, producer.id, resourceType, quantity, price)
            // Put resource in "escrow"--they'll get it back if the ask is withdrawn from the market
            val updatedResources = storedResources +
              (resourceType -> (storedResources.getOrElse(resourceType, 0) - quantity))

            tick(state = state.copy(producer = producer.copy(storedResources = updatedResources)))

          case PurchaseResource(resourceType, quantity, price) =>
            logProducerEvent(
              EventType.MarketTransaction,
              s"Purchased $quantity units of $resourceType at price $price"
            )

            val updatedResources = storedResources +
              (resourceType -> (storedResources.getOrElse(resourceType, 0) + quantity)) +
              (Money -> (storedResources.getOrElse(Money, 0) - (quantity*price)))
            tick(state.copy(producer = producer.copy(storedResources = updatedResources)))

          // As of right now, the resource for sale is in "escrow" so this just handles giving the actor money
          case ReceiveSalePayment(amount) =>
            logProducerEvent(
              EventType.Custom("SalePayment"),
              s"Received payment of $amount for sale"
            )

            val updatedResources = storedResources +
              (Money -> (storedResources.getOrElse(Money, 0) + amount))
            tick(state.copy(producer = producer.copy(storedResources = updatedResources)))

          // Deprecated for now
          case BuyFromSeller(seller, resourceType, quantity, price) =>
            logProducerEvent(
              EventType.MarketTransaction,
              s"Bought $quantity units of $resourceType at price $price directly from seller"
            )

            seller ! SellToBuyer(context.self, resourceType, quantity, price)
            val updatedResources = storedResources +
              (resourceType -> (storedResources.getOrElse(resourceType, 0) + quantity)) +
              (Money -> (storedResources.getOrElse(Money, 0) - Math.multiplyExact(quantity, price)))
            tick(state.copy(
              producer = producer.copy(storedResources = updatedResources)))

          // Deprecated for now
          case SellToBuyer(buyer, resourceType, quantity, price) =>
            logProducerEvent(
              EventType.MarketTransaction,
              s"Sold $quantity units of $resourceType at price $price directly to buyer"
            )

            val updatedResources = storedResources +
              (resourceType-> (storedResources.getOrElse(resourceType, 0) - quantity)) +
              (Money -> (storedResources.getOrElse(Money, 0) + Math.multiplyExact(quantity, price)))
            tick(state.copy(
              producer = producer.copy(storedResources = updatedResources)))

          case ShowInfo(replyTo) =>
            replyTo ! Some(InfoResponse(producer))
            Behaviors.same

          case HireWorkers(wage) =>
            if (producer.workers >= producer.maxWorkers) {
              // Do nothing
              Behaviors.same
            } else if (producer.storedResources.getOrElse(Money, 0) < producer.maxWorkers * producer.wage) {
              logProducerEvent(
                EventType.Custom("HiringFunds"),
                s"Not enough funds to hire workers. Issuing bond to raise capital."
              )
              // Would prefer to do this with the ask pattern but IssueBond may take multiple tries after which the callback is lost
              // So instead IssueBond takes care of going back into the loop of hiring workers
              context.self ! IssueBond(state.bankActor, producer.maxWorkers * producer.wage, 0.01)
              Behaviors.same
            } else {
              logProducerEvent(
                EventType.Custom("AttemptHiring"),
                s"Attempting to hire workers at wage ${producer.wage}"
              )

              context.ask(state.regionActor, Region.ReceiveWorkerBid(_, state.producer.id, producer.wage)) {
                case Success(Right(())) =>
                  AddWorker()
                case Success(Left(Some(co: Int))) =>
                  context.self ! SetWage(co)
                  context.self ! HireWorkers(co)
                  ActorNoOp()
                case _ =>
                  ActorNoOp()
              }
              Behaviors.same
            }

          case SetWage(wage) =>
            logProducerEvent(
              EventType.Custom("WageSet"),
              s"Set worker wage to $wage"
            )

            tick(state = state.copy(producer = producer.copy(wage = wage)))

          case MakeWorkerBid(sendTo, wage) =>
            logProducerEvent(
              EventType.Custom("WorkerBid"),
              s"Bidding for worker at wage $wage"
            )

            context.ask(sendTo, Region.ReceiveWorkerBid(_, state.producer.id, wage)) {
              case Success(true) =>
                AddWorker()
              case _ =>
                ActorNoOp()

            }
            Behaviors.same

          case AddWorker() =>
            logProducerEvent(
              EventType.WorkerHired,
              s"Hired new worker. Total workers now: ${producer.workers + 1}"
            )

            tick(state = state.copy(producer = producer.copy(workers = producer.workers + 1)))

          case IssueBond(sendTo, principal, interest) =>
            val bond = Bond(UUID.randomUUID().toString, principal, interest, principal, producer.id)

            logProducerEvent(
              EventType.BondIssued,
              s"Issuing bond ${bond.id} with principal $principal at ${interest * 100}% interest"
            )

            context.ask(sendTo, ReceiveBond(bond, _, context.self)) {
              case Success(Some(offered: Bond)) =>
                context.self ! HireWorkers(producer.wage)
                AddOutstandingBond(offered) //for now just accept whatever the bond is
              case _ =>
                ActorNoOp()
            }
            Behaviors.same

          case AddOutstandingBond(bond) =>
            logProducerEvent(
              EventType.Custom("BondReceived"),
              s"Received funds from bond ${bond.id}. Principal: ${bond.principal}"
            )

            val updatedResources = storedResources + (Money -> (storedResources.getOrElse(Money, 0) + bond.principal))

            val newOutstandingBonds = producer.outstandingBonds + (bond.id -> bond)

            tick(state = state.copy(producer = producer.copy(storedResources = updatedResources, outstandingBonds = newOutstandingBonds)))

          case PayBond(bond, amount, replyTo) =>
            val amountToPay = Math.min(storedResources.getOrElse(Money, 0), amount) // For now just have it pay what it can without defaults

            if (amountToPay < amount) {
              logProducerEvent(
                EventType.Custom("PartialPayment"),
                s"Making partial bond payment: $amountToPay of requested $amount"
              )
            } else {
              logProducerEvent(
                EventType.BondRepaid,
                s"Making bond payment of $amountToPay to bond ${bond.id}"
              )
            }

            val updatedBond = bond.copy(totalOutstanding = Math.round((bond.totalOutstanding - amountToPay)*bond.interestRate).toInt)
            val newOutstandingBonds = if (updatedBond.totalOutstanding <= 0) {
              logProducerEvent(
                EventType.BondRepaid,
                s"Bond ${bond.id} fully repaid"
              )
              producer.outstandingBonds - bond.id
            } else {
              producer.outstandingBonds + (bond.id -> updatedBond)
            }
            val updatedResources = storedResources + (Money -> (storedResources.getOrElse(Money, 0) - amountToPay))
            replyTo ! amountToPay
            tick(state = state.copy(producer = producer.copy(storedResources = updatedResources, outstandingBonds = newOutstandingBonds)))


          case _ =>
            Behaviors.same
        }
      }
    }

    Behaviors.withTimers { timers =>
      timers.startTimerWithFixedDelay("production", ProduceResource(), 8.second)
      timers.startTimerWithFixedDelay("worker-payment", PayWages(), 8.second)

      tick(state)
    }
  }

  def calculateProductionCost(state: ProducerActorState): Int = {
    val producer = state.producer

    // Calculate input costs
    val inputCosts = producer.inputs.map { case (resourceType, amountNeeded) =>
      val pricePerUnit = state.bidPrices.getOrElse(resourceType, 0)
      amountNeeded * pricePerUnit
    }.sum

    // Calculate labor costs
    val laborCosts = producer.workers * producer.wage

    // Calculate bond repayment costs
    val bondRepaymentCosts = if (producer.outstandingBonds.isEmpty) {
      0
    } else {
      producer.outstandingBonds.values.map { bond =>
        val interestPayment = (bond.totalOutstanding * bond.interestRate).toInt
        val principalPayment = Math.ceil(bond.totalOutstanding / 10.0).toInt // Assume paying 10% of principal per cycle
        interestPayment + principalPayment
      }.sum
    }

    // Calculate total cost and cost per unit
    val totalCost = inputCosts + laborCosts + bondRepaymentCosts
    val productionAmount = Math.max(1, producer.baseProduction * producer.workers) // Ensure we don't divide by zero
    val costPerUnit = Math.ceil(totalCost.toDouble / productionAmount).toInt

    costPerUnit
  }
}