package agents

import agents.EconAgent.CounterOffer
import agents.Producer._
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout
import middleware.GameInfo

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

case class ProducerActorState(
                           producer: Producer,
                           econActors: Map[String, ActorRef[BankActor.Command]],
                           regionActor: ActorRef[RegionActor.Command]
                             )

case class Producer(
                 id: String,
                 regionId: String,
                 workers: Int,
                 resourceProduced: ResourceType,
                 askPrice: Int,
                 storedResources: Map[ResourceType, Int],
                 bidPrices: Map[ResourceType, Int],
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
      askPrice = 1, // Figure out a good way to set this later, if it's 0 worried about some race conditions
      storedResources = Map[ResourceType, Int](),
      bidPrices = Map[ResourceType, Int](),
      wage = 0,
      maxWorkers = 10,
      baseProduction = baseProduction,
      inputs = Map(),
      multipliers = Map(),
      outstandingBonds = Map())
  }

  trait Command

  case class HireWorkers() extends Command

  case class ProcureResource(resourceType: ResourceType, qty: Int) extends Command

  case class SetAskPrice(price: Int) extends Command

  case class SetBidPrice(resourceType: ResourceType, price: Int) extends Command

}

object ProducerActor {

  case class InfoResponse(producer: Producer) extends GameInfo.InfoResponse {
    override val agent: Producer = producer
  }

  // TODO copy instances of ResourceProducer.Command to Producer.Command as refactor
  type Command = ResourceProducer.Command | GameActorCommand | Producer.Command

  implicit val timeout: Timeout = Timeout(3.seconds) // Define an implicit timeout for ask pattern

  def apply(state: ProducerActorState): Behavior[Command] = Behaviors.setup { context =>
    def tick(state: ProducerActorState): Behavior[Command] = {
      val producer = state.producer
      val resourceProduced = producer.resourceProduced
      val storedResources = producer.storedResources
      val askPrice = producer.askPrice
      val bidPrices = producer.bidPrices
      Behaviors.receive { (context, message) =>

        message match {

          case ReceiveBid(replyTo, resourceType, quantity, price) =>
            // TODO possibly DRY this up by creating a generic pure function that takes in certain arguments and using that
            if (resourceType != resourceProduced) {
              replyTo ! RejectBid(None)
              tick(state)
            } else {
              val availableToSell = Math.min(storedResources.getOrElse(resourceProduced, 0), quantity)
              if (availableToSell == 0) {
                replyTo ! RejectBid(None)
                tick(state) // No resources to sell, do nothing
              } else {
                if (price < askPrice) {
                  replyTo ! RejectBid(Some(CounterOffer(availableToSell, askPrice)))
                } else {
                  if (availableToSell < quantity) {
                    replyTo ! RejectBid(Some(CounterOffer(availableToSell, price)))
                  } else {
                    replyTo ! AcceptBid()
                  }
                }

                val updatedResources = storedResources +
                  (resourceProduced -> (storedResources.getOrElse(resourceProduced, 0) - quantity)) +
                  (Money -> (storedResources(Money) + Math.multiplyExact(quantity, price)))
                tick(state.copy(
                  producer = producer.copy(storedResources = updatedResources)))
              }
            }

          case PayWages() =>
            val totalPayment = Math.multiplyExact(producer.workers, producer.wage)
            if (totalPayment > storedResources.getOrElse(Money, 0)) {
              // Take out loan from bank or lose workers or something, still working on it
            }
            val updatedResources = storedResources +
              (Money -> (storedResources(Money) - totalPayment))
            tick(state.copy(producer = producer.copy(storedResources = updatedResources)))

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

            val updatedResources = producer.inputs.foldLeft(storedResources) {
              case (acc, (r, qty)) =>
                val newQty = storedResources.getOrElse(r, 0) - (producer.inputs.getOrElse(r, 0) * totalProduction)
                storedResources + (r -> newQty)
            } + (resourceProduced -> (storedResources(resourceProduced) + totalProduction))

            tick(state.copy(
              producer = producer.copy(storedResources = updatedResources)))

          case ProcureResource(resourceType, quantity) =>
            // For now, redundantly just sending itself a MakeBid message, will probably remove this intermediary
            context.self ! MakeBid(state.regionActor, resourceType, quantity, bidPrices.getOrElse(resourceType, 0))
            Behaviors.same

          // TODO possibly refactor according to my notes
          case MakeBid(sendTo, resourceType, quantity, price) =>
            context.ask(sendTo, ReceiveBid(_, resourceType, quantity, price)) {
              case Success(AcceptBid()) =>
                BuyFromSeller(resourceType, quantity, price)
              case (Success(RejectBid(Some(CounterOffer(qty, newPrice))))) =>
                // TODO add wages to total inputs once I switch wages to be per output rather than per time period
                val newInputsCost = bidPrices.values.sum - price + newPrice
                if (newInputsCost > askPrice) {
                  context.self ! SetAskPrice(newInputsCost + 1)
                  context.self ! SetBidPrice(resourceType, newPrice)
                }
                MakeBid(sendTo, resourceType, qty, newPrice)
              case Success(RejectBid(None)) =>
                ActorNoOp()
              case _ =>
                ActorNoOp()
            }
            Behaviors.same

          case ShowInfo(replyTo) =>
            replyTo ! Some(InfoResponse(producer))
            Behaviors.same

          case HireWorkers() =>
            if (producer.workers < producer.maxWorkers) {
              context.self ! MakeWorkerBid(state.regionActor, producer.wage)
            }
            Behaviors.same

          case MakeWorkerBid(sendTo, wage) =>
            context.ask(sendTo, RegionActor.ReceiveWorkerBid(_, state.producer.id, wage)) {
              case Success(true) =>
                AddWorker()
              case _ =>
                ActorNoOp()

            }
            Behaviors.same

          case AddWorker() =>
            tick(state = state.copy(producer = producer.copy(workers = producer.workers + 1)))

          case IssueBond(principal, interest, issueTo) =>
            state.econActors.get(issueTo) match {
              case Some(actorRef) =>
                val bond = Bond(UUID.randomUUID().toString, principal, interest, principal, issueTo, producer.id)
                context.ask(actorRef, ReceiveBond(bond, _, context.self)) {
                  case Success(true) =>
                    AddOutstandingBond(bond, issueTo)
                  case Success(false) =>
                    ActorNoOp()
                  case Failure(err) =>
                    println(s"failure in command IssueBond in farm ${producer.id}: ${err}")
                    ActorNoOp()
                  case _ =>
                    ActorNoOp()
                }
              case _ =>
                println(s"couldn't find actor ref for bank ${issueTo}")
            }
            Behaviors.same

          case AddOutstandingBond(bond, issuedTo) =>
            val updatedResources = storedResources + (Money -> (storedResources.getOrElse(Money, 0) + bond.principal))

            val newOutstandingBonds = producer.outstandingBonds + (bond.id -> bond)

            tick(state = state.copy(producer = producer.copy(storedResources = updatedResources, outstandingBonds = newOutstandingBonds)))

          case PayBond(bond, amount, replyTo) =>
            val amountToPay = Math.min(storedResources.getOrElse(Money, 0), amount) // For now just have it pay what it can without defaults
            val updatedBond = bond.copy(totalOutstanding = Math.round((bond.totalOutstanding - amountToPay)*bond.interestRate).toInt)
            val newOutstandingBonds = if (updatedBond.totalOutstanding <= 0) {
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
}