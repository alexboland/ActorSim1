import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout
import spray.json.DefaultJsonProtocol._

import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

case class Region(
               laborAssignments: Map[ActorRef[EconActorCommand], Int],
               storedResources: Map[ResourceType, Int],
               population: Int,
               baseProduction: Map[ResourceType, Double], //Production of spare resources that happens by default
               season: Region.Season,
               government: ActorRef[GovernmentActor.Command]
               )

object Region {
  trait Season {
    def multiplier: Double
    def next: Season
  }
  case object Spring extends Season {
    override def multiplier = 0.8
    override def next = Summer
  }
  case object Summer extends Season {
    override def multiplier = 1.5
    override def next = Autumn
  }
  case object Autumn extends Season {
    override def multiplier = 1.2
    override def next = Winter
  }
  case object Winter extends Season {
    override def multiplier = 0.3
    override def next = Spring
  }

  def newRandomRegion(govt: ActorRef[GovernmentActor.Command]): Region = {

    // TODO add code here for creating random resource base production amounts

    val foodProduction = (2 + Math.round(4 * Math.random())) * 0.25

    Region(
      laborAssignments = Map(),
      population = 100,
      storedResources = Map(
        Food -> 0
      ),
      baseProduction = Map(
        Food -> foodProduction
      ),
      season = Spring,
      government = govt
    )
  }
}

object RegionActor {
  trait Command extends EconActorCommand

  case class BuyFromSeller(resourceType: ResourceType, quantity: Int, price: Int) extends Command

  case class InfoResponse(region: Region) extends ManagerActor.Command

  case class DiscoverResource(resourceType: ResourceType, quantity: Int) extends Command

  case class InitializeRegion(region: Region) extends Command

  case class ConsumeFood() extends Command

  case class ChangePopulation() extends Command

  case class ReceiveWorkerBid(replyTo: ActorRef[EconActorCommand], price: Int) extends Command

  case class ProduceBaseResources() extends Command

  case class ChangeSeason() extends Command

  case class SellResourceToGovt(sellTo: ActorRef[GovernmentActor.Command], resourceType: ResourceType, qty: Int, price: Int) extends Command

  def apply(region: Region): Behavior[Command] = Behaviors.setup { context =>
    implicit val timeout: Timeout = Timeout(3.seconds) // Define an implicit timeout for ask pattern
    def tick(region: Region): Behavior[Command] = {
        Behaviors.receive { (context, message) =>
          message match {
            /*case ReceiveBid(resourceType, quantity, price) =>
              if (region.storedResources(resourceType) < quantity) {
                tick(region) // Insufficient resources to sell, do nothing
                // TODO create some kind of threshold to dictate how much capacity it wants to retain
              } else {
                val newStoredResources = region.storedResources + (resourceType -> (region.storedResources(resourceType) - quantity))
                tick(region.copy(storedResources = newStoredResources))
              }


            case DiscoverResource(resourceType, quantity) =>
              val newNaturalResources = region.baseProduction.updated(resourceType, region.baseProduction(resourceType) + quantity)
              tick(region.copy(baseProduction = newNaturalResources))*/

            case ProduceBaseResources() =>
              val newStoredResources = region.baseProduction.foldLeft(region.storedResources) { (updatedResources, entry) =>
                val output = Math.round(10 * Math.sqrt(region.population * (1.5 - Math.random())) * entry._2) // Square root with coefficient
                entry._1 match {
                  case Food =>
                    val foodProduced = Math.round(output * region.season.multiplier).toInt
                    updatedResources.updated(entry._1, updatedResources(entry._1) + foodProduced)
                  case _ =>
                    updatedResources.updated(entry._1, updatedResources(entry._1) + output.toInt)
                }
              }

              //This is just dummy logic to test it, I'll come up with a better formula soon
              if (newStoredResources(Food) > 200) {
                context.ask(region.government, GetBidPrice(_, Food)) {
                  case Success(Some(price)) =>
                    context.self ! SellResourceToGovt(region.government, Food, newStoredResources(Food) - 200, price)
                    ActorNoOp()
                  case Success(None) =>
                    ActorNoOp()
                  case Failure(exception) =>
                    println(s"EXCEPTION WITH GOVT: ${exception.toString}")
                    ActorNoOp()
                }
              }

              tick(region.copy(storedResources = newStoredResources))

            case SellResourceToGovt(sellTo, resourceType, qty, price) =>
              sellTo ! GovernmentActor.BuyResource(resourceType, qty)

              val newStoredResources = region.storedResources +
                (resourceType -> (region.storedResources(resourceType) - qty)) +
                (Money -> (region.storedResources.getOrElse(Money, 0) + (qty*price)))

              tick(region.copy(storedResources = newStoredResources))

            case ConsumeFood() =>
              val foodConsumed = region.population //still tweaking this idea
              println("===============")
              println(s"food consumed: ${foodConsumed}")
              println("===============")
              val updatedResources = region.storedResources + (Food -> Math.max(region.storedResources(Food) - foodConsumed, 0))

              val newPop = if (foodConsumed > region.storedResources(Food)) {
                Math.max(region.population - Math.round((foodConsumed - region.storedResources(Food))/5), 0)
              } else {
                region.population
              }

              println(s"newPop: ${newPop}")

              tick(region.copy(population = newPop, storedResources = updatedResources))

            case ChangePopulation() =>
              val baseGrowthFactor = 0.95f                       // Minimum growth factor
              val maxGrowthFactor = 1.05f                        // Maximum growth factor
              val growthRange = maxGrowthFactor - baseGrowthFactor  // Range of growth factors (0.1)
              val k = 5.0f                                       // Controls the steepness of the sigmoid
              val foodPerPopulation = region.storedResources(Food).toFloat / region.population.toFloat
              val x = foodPerPopulation - 1.0f                   // Center the sigmoid at foodPerPopulation = 1
              val s = 1.0f / (1.0f + Math.exp(-k * x).toFloat)   // Sigmoid function output between 0 and 1
              val growthFactor = baseGrowthFactor + growthRange * s

              val newPop = Math.round(region.population * growthFactor)

              println("================================")
              println(s"population: ${region.population}")
              println(s"growth factor: ${growthFactor}")
              println(s"new population: ${newPop}")
              println("================================")

              tick(region.copy(population = newPop))

            case ShowInfo(replyTo) =>
              println("region actor got ShowInfo command")
              replyTo ! InfoResponse(region)
              println("region actor replied to ShowInfo command")
              Behaviors.same

            case MakeBid(sendTo, resourceType, quantity, price) =>
              context.ask(sendTo, ReceiveBid(_, resourceType, quantity, price)) {
                case Success(AcceptBid()) =>
                  BuyFromSeller(resourceType, quantity, price)

                case Success(RejectBid()) =>
                  Behaviors.same
                  ActorNoOp()
                case Failure(_) =>
                  Behaviors.same
                  ActorNoOp()

              }
              Behaviors.same

            case BuyFromSeller(resourceType, quantity, price) =>
              val updatedResources = region.storedResources + (resourceType -> (region.storedResources(resourceType) + quantity), Money -> (region.storedResources(Money) - Math.multiplyExact(quantity, price)))
              tick(region.copy(storedResources = updatedResources))


            case ReceiveWorkerBid(replyTo, price) =>
              if (region.population - calculateAssignedWorkers(region) > 0) {
                replyTo ! AcceptWorkerBid() //TODO use price of food as floor for whether to accept
                tick(region.copy(laborAssignments = region.laborAssignments + (replyTo -> (region.laborAssignments.getOrElse(replyTo, 0) + 1))))
              } else {
                Behaviors.same
              }

            case ChangeSeason() =>
              tick(region.copy(season = region.season.next))

            case ActorNoOp() =>
              Behaviors.same


            case _ =>
              Behaviors.same
          }
        }
    }

    /*def initialize(region: Region): Behavior[Command] = {
      Behaviors.receive { (context, message) =>
        message match {
          case InitializeRegion(region) =>
            tick(region)
          case _ =>
            Behaviors.same
        }
      }
    }*/

    Behaviors.withTimers { timers =>
      timers.startTimerWithFixedDelay("consumption", ConsumeFood(), 10.second)
      timers.startTimerWithFixedDelay(key = "production", ProduceBaseResources(), 8.second)
      timers.startTimerWithFixedDelay("populationChange", ChangePopulation(), 30.second)
      timers.startTimerWithFixedDelay("seasonChange", ChangeSeason(), 40.second)


      tick(region)
    }
  }

  def calculateAssignedWorkers(region: Region): Int = region.laborAssignments.values.foldLeft(0)(_ + _)
}