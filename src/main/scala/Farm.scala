import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.util.Timeout
import spray.json.DefaultJsonProtocol._

import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

case class Farm(
  workers: Int,
  storedFood: Int,
  wage: Int,
  storedMoney: Int,
  maxWorkers: Int,
  baseProduction: Int,
  inputs: Map[ResourceType, Int],
  multipliers: Map[ResourceType, Int],
) extends ResourceProducer {
  override val resourceProduced: ResourceType = Food
}

object Farm {
  def newFarm(baseProduction: Int): Farm = {
    Farm(
      workers = 0,
      storedFood = 0,
      wage = 0,
      storedMoney = 0,
      maxWorkers = 10,
      baseProduction = baseProduction,
      inputs = Map(),
      multipliers = Map())
  }
}

object FarmActor {

  case class InfoResponse(farm: Farm) extends GameInfo.InfoResponse {
    override val agent = farm
  }

  def apply(farm: Farm): Behavior[ResourceProducerCommand] = Behaviors.setup { context =>
    implicit val timeout: Timeout = Timeout(3.seconds) // Define an implicit timeout for ask pattern
    def tick(farm: Farm): Behavior[ResourceProducerCommand] = {
      Behaviors.receive { (context, message) =>
        message match {
          case ReceiveBid(replyTo, resourceType, quantity, price) =>
            if (resourceType != Food) {
              replyTo ! RejectBid()
              tick(farm)
            } else if (farm.storedFood < quantity) {
              replyTo ! RejectBid()
              tick(farm) // Insufficient resources to sell, do nothing
              // TODO create some kind of threshold to dictate how much capacity it wants to retain
            } else {
              // Currently dumb behavior where it doesn't negotiate
              replyTo ! AcceptBid()
              tick(farm.copy(storedFood = farm.storedFood - quantity, storedMoney = farm.storedMoney + Math.multiplyExact(quantity, price)))
            }

          case PayWages() =>
            val totalPayment = Math.multiplyExact(farm.workers, farm.wage)
            if (totalPayment > farm.storedMoney) {
              // Take out loan from bank or lose workers or something, still working on it
            }
            tick(farm.copy(storedMoney = farm.storedMoney - totalPayment))

          case ProduceResource() =>
            tick(farm.copy(storedFood = farm.storedFood + Math.multiplyExact(farm.baseProduction, farm.workers)))

          /*case ShowInfo(replyTo) =>
            replyTo ! ResourceProducer.InfoResponse(farm)
            Behaviors.same*/

          case MakeWorkerBid(sendTo, wage) =>
            context.ask(sendTo, RegionActor.ReceiveWorkerBid(_, wage)) {
              case Success(AcceptWorkerBid()) =>
                Behaviors.same
                AddWorker()
              case Success(RejectWorkerBid()) =>
                Behaviors.same
                ActorNoOp()
              case Failure(_) =>
                Behaviors.same
                ActorNoOp()

            }
            Behaviors.same

          case AddWorker() =>
            tick(farm.copy(workers = farm.workers + 1))

          case _ =>
            Behaviors.same
        }
      }
    }

    Behaviors.withTimers { timers =>
      timers.startTimerWithFixedDelay("production", ProduceResource(), 8.second)

      tick(farm)
    }
  }
}