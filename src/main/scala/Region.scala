import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout
import spray.json.DefaultJsonProtocol._

import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

case class RegionActorState(
                           region: Region,
                           governmentActor: ActorRef[GovernmentActor.Command],
                           econActorIds: Map[ActorRef[ResourceProducerCommand], String]
                           )

case class Region(
               laborAssignments: Map[String, Int],
               storedResources: Map[ResourceType, Int],
               population: Int,
               baseProduction: Map[ResourceType, Double], //Production of spare resources that happens by default
               season: Region.Season
               ) extends GameAgent

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

  def newRandomRegion(): Region = {

    // TODO add code here for creating random resource base production amounts

    val foodProduction = (2 + Math.round(4 * Math.random())) * 0.25
    val copperProduction = Math.round(6 * Math.random()) * 0.25
    val woodProduction = Math.round(6 * Math.random()) * 0.25

    Region(
      laborAssignments = Map(),
      population = 100,
      storedResources = Map(
        Food -> 0
      ),
      baseProduction = Map(
        Food -> foodProduction,
        Copper -> copperProduction,
        Wood -> woodProduction
      ),
      season = Spring,
    )
  }
}

object RegionActor {
  trait Command extends EconActorCommand

  case class BuyFromSeller(resourceType: ResourceType, quantity: Int, price: Int) extends Command

  case class InfoResponse(region: Region) extends GameInfo.InfoResponse {
    val agent = region
  }

  case class FullInfoResponse(region: Region, gameAgents: List[GameAgent]) extends GameInfo.InfoResponse {
    val agent = region
  }

  case class DiscoverResource(resourceType: ResourceType, quantity: Int) extends Command

  case class InitializeRegion(region: Region) extends Command

  case class ConsumeFood() extends Command

  case class ChangePopulation() extends Command

  case class ReceiveWorkerBid(replyTo: ActorRef[EconActorCommand], price: Int) extends Command

  case class ProduceBaseResources() extends Command

  case class ChangeSeason() extends Command

  case class SellResourceToGovt(resourceType: ResourceType, qty: Int, price: Int) extends Command
  case class BuyResourceFromGovt(resourceType: ResourceType, qty: Int, price: Int) extends Command

  case object BuildFarm extends Command

  case class ShowFullInfo(replyTo: ActorRef[GameInfo.InfoResponse]) extends Command

  def apply(state: RegionActorState): Behavior[Command] = Behaviors.setup { context =>
    implicit val timeout: Timeout = Timeout(3.seconds) // Define an implicit timeout for ask pattern
    def tick(state: RegionActorState): Behavior[Command] = {
        Behaviors.receive { (context, message) =>
          val region = state.region
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
                    updatedResources.updated(entry._1, updatedResources.getOrElse(entry._1, 0) + foodProduced)
                  case _ =>
                    updatedResources.updated(entry._1, updatedResources.getOrElse(entry._1, 0) + output.toInt)
                }
              }

              //This is just dummy logic to test it, I'll come up with a better formula soon
              if (newStoredResources(Food) > region.population * 1.5) {
                context.ask(state.governmentActor, GetBidPrice(_, Food)) {
                  case Success(Some(price)) =>
                    context.self ! SellResourceToGovt(Food, newStoredResources(Food) - Math.round(region.population * 1.5).toInt, price)
                    ActorNoOp()
                  case Success(None) =>
                    ActorNoOp()
                  case Failure(exception) =>
                    println(s"EXCEPTION WITH GOVT: ${exception.toString}")
                    ActorNoOp()
                }
              }

              tick(state.copy(region = region.copy(storedResources = newStoredResources)))

            case SellResourceToGovt(resourceType, qty, price) =>
              state.governmentActor ! GovernmentActor.BuyResource(resourceType, qty)

              val newStoredResources = region.storedResources +
                (resourceType -> (region.storedResources.getOrElse(resourceType, 0) - qty)) +
                (Money -> (region.storedResources.getOrElse(Money, 0) + (qty*price)))

              tick(state.copy(region = region.copy(storedResources = newStoredResources)))

            case BuyResourceFromGovt(resourceType, qty, price) =>
              state.governmentActor ! GovernmentActor.BuyResource(resourceType, qty)

              val newStoredResources = region.storedResources +
                (resourceType -> (region.storedResources.getOrElse(resourceType, 0) + qty)) +
                (Money -> (region.storedResources.getOrElse(Money, 0) - (qty * price)))

              tick(state.copy(region = region.copy(storedResources = newStoredResources)))

            case ConsumeFood() =>
              val foodConsumed = region.population //still tweaking this idea
              val updatedResources = region.storedResources + (Food -> Math.max(region.storedResources(Food) - foodConsumed, 0))

              val newPop = if (foodConsumed > region.storedResources(Food)) {
                Math.max(region.population - Math.round((foodConsumed - region.storedResources(Food))/5), 0)
              } else {
                region.population
              }

              if (updatedResources(Food) < region.population * 1.5) {
                context.ask(state.governmentActor, GetAskPrice(_, Food)) {
                  case Success(Some(price)) =>
                    val qtyToBuy = Math.min(updatedResources.getOrElse(Money, 0)/price, Math.round(region.population * 1.5f) - updatedResources(Food))
                    context.self ! BuyResourceFromGovt(Food, qtyToBuy, price)
                    ActorNoOp()
                  case Success(None) =>
                    ActorNoOp()
                  case Failure(exception) =>
                    println(s"EXCEPTION WITH GOVT: ${exception.toString}")
                    ActorNoOp()
                }
              }


              tick(state.copy(region = region.copy(population = newPop, storedResources = updatedResources)))

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

              tick(state.copy(region = region.copy(population = newPop)))

            case ShowInfo(replyTo) =>
              println("region actor got ShowInfo command")
              replyTo ! Some(InfoResponse(region))
              println("region actor replied to ShowInfo command")
              Behaviors.same

            case ShowFullInfo(replyTo) =>
              implicit val scheduler = context.system.scheduler
              implicit val ec = context.executionContext

              val futures = state.econActorIds.map({
                case (actor, uuid) =>
                  actor.ask(ShowInfo(_)) // Need to change this so the response to showinfo is actually the info
                  //context.ask(actor, ShowInfo(_))
              })

              val aggregateInfo = Future.sequence(futures)

              aggregateInfo.onComplete({
                case Success(iter) =>
                  val info = iter.flatMap(_.map(_.agent)).toList
                  replyTo ! FullInfoResponse(region, info)
                case _ =>
                  println("whatever...")

              })
              Behaviors.same

            case MakeBid(sendTo, resourceType, quantity, price) =>
              context.ask(sendTo, ReceiveBid(_, resourceType, quantity, price)) {
                case Success(AcceptBid()) =>
                  BuyFromSeller(resourceType, quantity, price)
                case Success(RejectBid()) =>
                  ActorNoOp()
                case Failure(_) =>
                  ActorNoOp()
                case _ =>
                  ActorNoOp()

              }
              Behaviors.same

            case BuyFromSeller(resourceType, quantity, price) =>
              val updatedResources = region.storedResources + (resourceType -> (region.storedResources.getOrElse(resourceType, 0) + quantity), Money -> (region.storedResources.getOrElse(Money, 0) - Math.multiplyExact(quantity, price)))
              tick(state.copy(region = region.copy(storedResources = updatedResources)))


            case ReceiveWorkerBid(replyTo, price) =>
              if (region.population - calculateAssignedWorkers(region) > 0) {
                replyTo ! AcceptWorkerBid() //TODO use price of food as floor for whether to accept
                tick(state.copy(
                  region = region.copy(
                    laborAssignments = region.laborAssignments + // TODO make this fetch more safe
                      (state.econActorIds(replyTo) -> (region.laborAssignments.getOrElse(state.econActorIds(replyTo), 0) + 1)))))
              } else {
                Behaviors.same
              }

            case ChangeSeason() =>
              tick(state.copy(region = region.copy(season = region.season.next)))

            case BuildFarm =>
              val farm = Farm.newFarm(2)
              val uuid = UUID.randomUUID()
              val actorRef = context.spawn(FarmActor(farm), uuid.toString)
              tick(state.copy(
                region = region.copy(laborAssignments = region.laborAssignments + (uuid.toString -> 0)),
                econActorIds = state.econActorIds + (actorRef -> uuid.toString)))

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


      tick(state)
    }
  }

  def calculateAssignedWorkers(region: Region): Int = region.laborAssignments.values.foldLeft(0)(_ + _)
}