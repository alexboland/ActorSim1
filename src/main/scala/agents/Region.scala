package agents

import agents.EconAgent.CounterOffer
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout
import middleware.GameInfo

import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

case class RegionActorState(
                           region: Region,
                           governmentActor: ActorRef[GovernmentActor.Command],
                           econActors: Map[String, List[EconActor]],
                           )

case class Region(
               id: String,
               location: Region.Location,
               // TODO consider getting rid of laborAssignments: the information can easily be implied by just having a spareWorkers number instead
               assignedWorkers: Int,
               storedResources: Map[ResourceType, Int],
               population: Int,
               baseProduction: Map[ResourceType, Double], //Production of spare resources that happens by default
               season: Region.Season
               ) extends GameAgent

object Region {

  case class Location(x: Int, y: Int)

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

  def newRandomRegion(x: Int, y: Int): Region = {

    // TODO add code here for creating random resource base production amounts

    val foodProduction = (2 + Math.round(4 * Math.random())) * 0.25
    val copperProduction = Math.round(6 * Math.random()) * 0.25
    val woodProduction = Math.round(6 * Math.random()) * 0.25

    Region(
      id = UUID.randomUUID.toString(),
      location = Location(x = x, y = y),
      assignedWorkers = 0,
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
  trait RegionCommand

  case class InfoResponse(region: Region) extends GameInfo.InfoResponse {
    val agent = region
  }

  case class FullInfoResponse(region: Region, gameAgents: List[GameAgent]) extends GameInfo.InfoResponse {
    val agent = region
  }

  case class DiscoverResource(resourceType: ResourceType, quantity: Int) extends RegionCommand

  case class InitializeRegion(region: Region) extends RegionCommand

  case class ConsumeFood() extends RegionCommand

  case class ChangePopulation() extends RegionCommand

  case class ReceiveWorkerBid(replyTo: ActorRef[Boolean], agentId: String, price: Int) extends RegionCommand

  case class ProduceBaseResources() extends RegionCommand

  case class ChangeSeason() extends RegionCommand

  case class SellResourceToGovt(resourceType: ResourceType, qty: Int, price: Int) extends RegionCommand
  case class BuyResourceFromGovt(resourceType: ResourceType, qty: Int, price: Int) extends RegionCommand

  case class BuildFarm() extends RegionCommand

  case class BuildBank() extends RegionCommand

  case class BuildMarket() extends RegionCommand

  case class BuildProducer(producer: Producer) extends RegionCommand

  case class ShowFullInfo(replyTo: ActorRef[Option[GameInfo.InfoResponse]]) extends RegionCommand

  case class SpawnFounder() extends RegionCommand
  
  type Command = RegionCommand | EconAgent.Command | GameActorCommand

  implicit val timeout: Timeout = Timeout(3.seconds) // Define an implicit timeout for ask pattern

  def apply(state: RegionActorState): Behavior[Command] = Behaviors.setup { context =>
    def tick(state: RegionActorState): Behavior[Command] = {

      implicit val scheduler = context.system.scheduler
      implicit val ec = context.executionContext

      Behaviors.receive { (context, message) =>
        val region = state.region
        val econActors = state.econActors
        message match {
          // For the time being, bids from/to market will be mediated through region agent
          case ReceiveBid(replyTo, resourceType, quantity, price) =>
            econActors.getOrElse("market", List()).headOption match {
              case Some(marketActor) =>
                if (replyTo != marketActor) {
                  marketActor ! ReceiveBid(replyTo, resourceType, quantity, price)
                } else {
                  // If the bid is coming from the market, search for appropriate producers to forward things to
                  // For now, however, just send bids to all producers: will worry about efficiency later

                  //Collect all responses and look for one that provides the right quantity and price
                  // If none exist, look for the cheapest counter-offer and return that to the market.
                  // The market can decide from there whether to accept and how to procure the rest
                  val futures = econActors.getOrElse("producers", List()).map({
                    case (actor) =>
                      actor.ask(ReceiveBid(_, resourceType, quantity, price))
                  })

                  val aggregateResponses = Future.sequence(futures)

                  aggregateResponses.onComplete({
                    case Success(responses) =>
                      val bestResponse = responses.collectFirst {
                        case accept @ AcceptBid() => accept
                      }.getOrElse {
                        responses.collect { case RejectBid(Some(co)) => co }
                          .minByOption(_.price) // Find the lowest price
                          .map(co => RejectBid(Some(co))) // Wrap it back in RejectBid
                          .getOrElse(RejectBid(None)) // If nothing is found, return RejectBid(None)
                      }
                      replyTo ! bestResponse
                    case _ =>
                      println("whatever...")

                  })
                  Behaviors.same

                  // For now, at least have founder objects receive these bids
                  econActors.getOrElse("founders", List()).foreach { actorRef =>
                    actorRef ! ReceiveBid(replyTo, resourceType, quantity, price)
                  }

                }
                Behaviors.same
              case None =>
                Behaviors.same // This should never happen anyway
            }

          case ReceiveAsk(replyTo, resourceType, quantity, price) =>
            econActors.getOrElse("market", List()).headOption match {
              case Some(marketActor) =>
                if (replyTo != marketActor) {
                  marketActor ! ReceiveAsk(replyTo, resourceType, quantity, price)
                } else {
                  // If the ask is coming from the market, search for appropriate resource producers to forward things to
                  // But there's no plan to use this functionality here, still...
                }
                Behaviors.same
              case None =>
                Behaviors.same // This should never happen anyway
            }

          /*case DiscoverResource(resourceType, quantity) =>
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
                case Success(Some(price: Int)) =>
                  context.self ! SellResourceToGovt(Food, newStoredResources(Food) - Math.round(region.population * 1.5).toInt, price)
                  ActorNoOp()
                case Success(_) =>
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
                case Success(Some(price: Int)) =>
                  val qtyToBuy = Math.min(updatedResources.getOrElse(Money, 0)/price, Math.round(region.population * 1.5f) - updatedResources(Food))
                  context.self ! BuyResourceFromGovt(Food, qtyToBuy, price)
                  ActorNoOp()
                case Success(_) =>
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
            replyTo ! Some(InfoResponse(region))
            Behaviors.same

          case ShowFullInfo(replyTo) =>
            // TODO create a more structured version of this

            val futures = econActors.values.flatten.toList.map({
              case (actor) =>
                actor.ask(ShowInfo.apply)
            })

            val aggregateInfo = Future.sequence(futures)

            aggregateInfo.onComplete({
              case Success(iter: Iterable[Option[GameInfo.InfoResponse]]) =>
                val info = iter.flatMap(_.map(_.agent)).toList
                replyTo ! Some(FullInfoResponse(region, info))
              case _ =>
                println("whatever...")

            })
            Behaviors.same

          case MakeBid(sendTo, resourceType, quantity, price) =>
            context.ask(sendTo, ReceiveBid(_, resourceType, quantity, price)) {
              case Success(AcceptBid()) =>
                BuyFromSeller(sendTo, resourceType, quantity, price)
              case Success(RejectBid(None)) =>
                ActorNoOp()
              case Failure(_) =>
                ActorNoOp()
                // TODO add case of RejectBid(Some(...
              case _ =>
                ActorNoOp()

            }
            Behaviors.same

          case BuyFromSeller(seller, resourceType, quantity, price) =>
            seller ! SellToBuyer(context.self, resourceType, quantity, price)
            val updatedResources = region.storedResources + (resourceType -> (region.storedResources.getOrElse(resourceType, 0) + quantity), Money -> (region.storedResources.getOrElse(Money, 0) - Math.multiplyExact(quantity, price)))
            tick(state.copy(region = region.copy(storedResources = updatedResources)))


          case ReceiveWorkerBid(replyTo, agentId, price) =>
            if (region.population - region.assignedWorkers > 0) {
              replyTo ! true //TODO use price of food as floor for whether to accept
              tick(state.copy(
                region = region.copy(
                  assignedWorkers = region.assignedWorkers + 1)))
            } else {
              Behaviors.same
            }

          case ChangeSeason() =>
            tick(state.copy(region = region.copy(season = region.season.next)))

          case BuildBank() =>
            val bank = Bank.newBank(region.id)
            val actorRef = context.spawn(BankActor(BankActorState(bank, Map())), bank.id)
            val updatedFounders = actorRef :: econActors.getOrElse("founders", List())
            tick(state.copy(econActors = econActors + ("founders" -> updatedFounders)))

          case BuildMarket() =>
            val market = Market.newMarket(region.id)
            val actorRef = context.spawn(MarketActor(MarketActorState(market)), market.id)
            tick(state.copy(econActors = econActors + ("market" -> List(actorRef))))

          case BuildProducer(producer) =>
            val actorRef = context.spawn(ProducerActor(ProducerActorState(producer, Map(), context.self)), producer.id)
            val updatedProducers = actorRef :: econActors.getOrElse("producers", List())
            tick(state.copy(econActors = econActors + ("producers" -> updatedProducers)))

          case SpawnFounder() =>
            val newId = UUID.randomUUID().toString
            val newFounder = Founder(newId, region.id, None)
            val actorRef = context.spawn(FounderActor(FounderActorState(newFounder, context.self)), newId)
            val updatedFounders = actorRef :: econActors.getOrElse("founders", List())
            val updatedActors = econActors + ("founders" -> updatedFounders)
            tick(state.copy(econActors = updatedActors))

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

      // Spawn the initial founder
      timers.startSingleTimer("spawn_founder", SpawnFounder(), 2.second)


      tick(state)
    }
  }
}