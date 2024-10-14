package agents

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

import scala.concurrent.duration.DurationInt

case class Factory(
                 workers: Int,
                 resourceProduced: ResourceType,
                 maxWorkers: Int,
                 storedResources: Map[ResourceType, Int],
                 inputs: Map[ResourceType, Int],
                 multipliers: Map[ResourceType, Int],
                 baseProduction: Int,
               ) extends ResourceProducer

object FactoryActor {
  def apply(factory: Factory): Behavior[ResourceProducerCommand] = Behaviors.setup { context =>

    def tick(factory: Factory): Behavior[ResourceProducerCommand] = {
      Behaviors.receive { (context, message) =>
        message match {
          case ProduceResource() =>
            val producible = factory.storedResources.map {
              case (resourceType, qty) => (resourceType, Math.floorDiv(qty, factory.inputs(resourceType)))
            }.minBy(_._2)._2

            val newStoredResources = factory.storedResources.map {
              case (factory.resourceProduced, qty) => (factory.resourceProduced, Math.addExact(qty, producible))
              case (resourceType, qty) => (resourceType, factory.inputs(resourceType) - Math.multiplyExact(producible, factory.inputs(resourceType)))
            }

            tick(factory.copy(storedResources = newStoredResources))

          /*case ShowInfo(replyTo) =>
            replyTo ! ResourceProducer.InfoResponse(factory)
            Behaviors.same*/

          case _ =>
            Behaviors.same
        }
      }
    }

    Behaviors.withTimers { timers =>
      timers.startTimerWithFixedDelay("production", ProduceResource(), 8.second)

      tick(factory)
    }
  }
}