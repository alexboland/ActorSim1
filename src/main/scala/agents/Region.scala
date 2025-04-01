package agents

import agents.EconAgent.CounterOffer
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout
import middleware.{EventType, GameEvent, GameEventService, GameInfo}

import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

case class RegionActorState(
                           region: Region,
                           econAgentIds: Map[String, List[String]],
                           econActors: Map[String, EconActor]
                           )

case class Region(
               id: String,
               location: Region.Location,
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

    // random base production amounts
    // TODO factor this in to producer actors
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

  case class FullInfoResponse(
    region: Region,
    producers: List[Producer],
    founders: List[Founder],
    bank: Option[Bank],
    market: Option[Market]) extends GameInfo.InfoResponse {
    val agent = region
  }

  case class DiscoverResource(resourceType: ResourceType, quantity: Int) extends RegionCommand

  case class InitializeRegion(region: Region) extends RegionCommand

  case class ConsumeFood() extends RegionCommand

  case class ChangePopulation() extends RegionCommand

  case class ReceiveWorkerBid(replyTo: ActorRef[Either[Option[Int], Unit]], agentId: String, price: Int) extends RegionCommand

  case class ProduceBaseResources() extends RegionCommand

  case class ChangeSeason() extends RegionCommand

  case class SellResourceToGovt(resourceType: ResourceType, qty: Int, price: Int) extends RegionCommand
  case class BuyResourceFromGovt(resourceType: ResourceType, qty: Int, price: Int) extends RegionCommand

  case class SellSpareResources() extends RegionCommand

  case class BuildFarm() extends RegionCommand

  case class BuildBank() extends RegionCommand

  case class BuildMarket() extends RegionCommand

  case class BuildProducer(producer: Producer) extends RegionCommand

  case class ShowFullInfo(replyTo: ActorRef[Option[GameInfo.InfoResponse]]) extends RegionCommand

  case class SpawnFounder() extends RegionCommand

  case class UnassignWorkers(qty: Int) extends RegionCommand

  case class AssignWorkers(qty: Int) extends RegionCommand

  type Command = RegionCommand | EconAgent.Command | GameActorCommand | BankingCommand | Market.Command

  implicit val timeout: Timeout = Timeout(3.seconds) // Define an implicit timeout for ask pattern

  def apply(state: RegionActorState): Behavior[Command] = Behaviors.setup { context =>
    // Create event service
    val eventService = GameEventService(context)

    def tick(state: RegionActorState): Behavior[Command] = {

      implicit val scheduler = context.system.scheduler
      implicit val ec = context.executionContext

      // Helper function to log events, reducing repetition
      def logEvent(eventType: EventType, eventText: String): Unit = {
        eventService.logEvent(
          agentId = state.region.id,
          regionId = state.region.id,
          eventType = eventType,
          eventText = eventText
        )
      }

      Behaviors.receive { (context, message) =>
        val region = state.region
        val econAgentIds = state.econAgentIds
        val econActors = state.econActors
        message match {
          // For encapsulation, bids/asks from producers will be mediated through region agent into the market
          case ReceiveBid(replyTo, bidderId, resourceType, quantity, price) =>
            econAgentIds.getOrElse("market", List()).headOption.foreach { marketId =>
              if (bidderId != marketId) {
                econActors.get(marketId).foreach { marketActor =>
                  marketActor ! ReceiveBid(replyTo, bidderId, resourceType, quantity, price)
                }
              } else {
                // If the market is "making a bid" it's to forward unmet demand to founders
                econAgentIds.get("founders").foreach(_.foreach { founderId =>
                  econActors.get(founderId).foreach { founderActor =>
                    founderActor ! ReceiveBid(replyTo, bidderId, resourceType, quantity, price)
                  }
                })
              }
            }
            Behaviors.same

          case ReceiveAsk(replyTo, askerId, resourceType, quantity, price) =>
            econAgentIds.getOrElse("market", List()).headOption.foreach { marketId =>
              if (askerId != marketId) {
                econActors.get(marketId).foreach { marketActor =>
                  marketActor ! ReceiveAsk(replyTo, askerId, resourceType, quantity, price)
                }
              }
            }
            Behaviors.same

          case GetBidPrice(replyTo, resourceType) =>
            econAgentIds.get("market").flatMap(_.headOption).map { marketId =>
              econActors.get(marketId)
                .collect { case actor: ActorRef[MarketActor.Command] => actor }.map { marketActor =>
                  context.ask(marketActor, GetBidPrice(_, resourceType)) {
                    case Success(bidOpt: Option[Int]) =>
                      replyTo ! bidOpt
                      ActorNoOp()
                    case _ =>
                      println("Some kind of error...")
                      ActorNoOp()
                  }
                  Behaviors.same
                }.getOrElse(Behaviors.same)
            }.getOrElse(Behaviors.same)

          case GetAskPrice(replyTo, resourceType) =>
            econAgentIds.getOrElse("market", List()).headOption.foreach {id =>
              econActors.get(id).foreach {
                case (marketActor: ActorRef[MarketActor.Command]) =>
                  context.ask(marketActor, GetAskPrice(_, resourceType)) {
                    case Success(askOpt: Option[Int]) =>
                      replyTo ! askOpt
                      ActorNoOp()
                    case _ =>
                      println("Some kind of error...")
                      ActorNoOp()
                  }
              }
            }
            Behaviors.same

          case ReceiveBond(bond, replyTo, issuedFrom) =>
            // For now, if the bond is being issued by a (the) bank, forward to government (central bank)
            // Otherwise, forward to the one bank in the region
            // This will almost certainly change in future iterations
            econAgentIds.get("bank").flatMap(_.headOption).foreach { bankId =>
              if (bond.debtorId == bankId) {
                econAgentIds.getOrElse("government", List()).headOption.foreach { id =>
                  econActors.get(id) match {
                    case Some(govtActor: ActorRef[GovernmentActor.Command]) =>
                      govtActor ! ReceiveBond(bond, replyTo, issuedFrom)
                  }
                }
              } else {
                econActors.get(bankId) match {
                  case Some(bankActor: ActorRef[BankActor.Command]) =>
                    bankActor ! ReceiveBond(bond, replyTo, issuedFrom)
                }
              }
            }
            Behaviors.same

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

            // Log resource production events
            newStoredResources.foreach { case (resourceType, quantity) =>
              val previousQuantity = region.storedResources.getOrElse(resourceType, 0)
              val produced = quantity - previousQuantity
              if (produced > 0) {
                logEvent(
                  EventType.ResourceProduced,
                  s"Produced ${produced} ${resourceType} (Total: ${quantity})"
                )
              }
            }

            //This is just dummy logic to test it, I'll come up with a better formula soon
            // TODO move this logic to a new Command called sellSpareResources for better delegation/legibility
            econAgentIds.getOrElse("market", List()).headOption.foreach { id =>
              econActors.get(id).foreach { marketActor =>
                if (newStoredResources(Food) > region.population * 1.5) {
                  context.ask(marketActor, GetBidPrice(_, Food)) {
                    case Success(Some(price: Int)) =>
                      econAgentIds.getOrElse("market", List()).headOption.map { id =>
                        econActors.get(id).map { marketActor =>
                          MakeAsk(marketActor, Food, newStoredResources(Food) - Math.round(region.population * 1.5).toInt, price)
                        }.getOrElse(ActorNoOp())
                      }.getOrElse(ActorNoOp())
                    case Success(_) =>
                      ActorNoOp()
                    case Failure(exception) =>
                      println(s"EXCEPTION: ${exception.toString}")
                      ActorNoOp()
                  }
                }

                (region.storedResources - Food).foreach { (rt, qty) =>
                  context.ask(marketActor, GetBidPrice(_, rt)) {
                    case Success(Some(price: Int)) =>
                      MakeAsk(marketActor, rt, qty, price)
                    case _ =>
                      ActorNoOp()
                  }
                }
              }
            }

            tick(state.copy(region = region.copy(storedResources = newStoredResources)))

          case ConsumeFood() =>
            val foodConsumed = region.population //still tweaking this idea
            val updatedResources = region.storedResources + (Food -> Math.max(region.storedResources(Food) - foodConsumed, 0))

            // Log food consumption
            logEvent(
              EventType.ResourceConsumed,
              s"Consumed ${foodConsumed} food (Remaining: ${updatedResources(Food)})"
            )

            val newPop = if (foodConsumed > region.storedResources(Food)) {
              val popDecrease = Math.round((foodConsumed - region.storedResources(Food))/5)
              val newPopValue = Math.max(region.population - popDecrease, 0)

              // Log population decrease due to food shortage
              if (popDecrease > 0) {
                logEvent(
                  EventType.PopulationChanged,
                  s"Population decreased by ${popDecrease} due to food shortage (New: ${newPopValue})"
                )
              }

              newPopValue
            } else {
              region.population
            }

            if (updatedResources(Food) < region.population * 1.5) {
              econAgentIds.getOrElse("market", List()).headOption.foreach { id =>
                econActors.get(id).foreach { marketActor =>
                  context.ask(marketActor, GetAskPrice(_, Food)) {
                    case Success(Some(price: Int)) =>
                      val qtyToBuy = Math.min(updatedResources.getOrElse(Money, 0) / price, Math.round(region.population * 1.5f) - updatedResources(Food))
                      context.self ! MakeBid(marketActor, Food, qtyToBuy, price)
                      ActorNoOp()
                    case _ =>
                      ActorNoOp()
                  }
                }
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
            val popChange = newPop - region.population

            // Log population change
            if (popChange != 0) {
              logEvent(
                EventType.PopulationChanged,
                s"Population ${if (popChange > 0) "increased" else "decreased"} by ${Math.abs(popChange)} (New: ${newPop})"
              )
            }

            tick(state.copy(region = region.copy(population = newPop)))

          case ShowInfo(replyTo) =>
            replyTo ! Some(InfoResponse(region))
            Behaviors.same

          case ShowFullInfo(replyTo) =>
            // Get producers list
            val producersFutures = econAgentIds.getOrElse("producers", List())
              .flatMap(id => econActors.get(id).map(actor => actor.ask(ShowInfo.apply)))

            // Get founders list
            val foundersFutures = econAgentIds.getOrElse("founders", List())
              .flatMap(id => econActors.get(id).map(actor => actor.ask(ShowInfo.apply)))

            // Get the first bank
            val bankFuture = econAgentIds.getOrElse("bank", List()).headOption.flatMap(id => {
              econActors.get(id).map { actor =>
                actor.ask(ShowInfo.apply)
              }
            }).getOrElse(Future.successful(None))

            // Get the first market
            val marketFuture = econAgentIds.getOrElse("market", List()).headOption.flatMap(id => {
              econActors.get(id).map { actor =>
                actor.ask(ShowInfo.apply)
              }
            }).getOrElse(Future.successful(None))

            // Combine all futures
            val producersAggregate = Future.sequence(producersFutures)
            val foundersAggregate = Future.sequence(foundersFutures)

            val combinedFuture = for {
              producers <- producersAggregate
              founders <- foundersAggregate
              bank <- bankFuture
              market <- marketFuture
            } yield (producers, founders, bank, market)

            combinedFuture.onComplete {
              case Success((producersInfo, foundersInfo, bankInfo, marketInfo)) =>
                // Extract and cast producers to Producer type
                val producersData = try {
                  producersInfo.flatMap(info => {
                    info.map(infoResponse => {
                      infoResponse.agent
                    })
                  }).collect {
                    case agent: Producer =>
                      agent  // Cast to Producer
                  }.toList
                } catch {
                  case e: Exception =>
                    println(s"[DEBUG] Exception processing producers: ${e.getMessage}")
                    List.empty
                }

                // Extract and cast founders to Founder type
                val foundersData = try {
                  foundersInfo.flatMap(info => {
                    info.map(infoResponse => {
                      infoResponse.agent
                    })
                  }).collect {
                    case agent: Founder =>
                      agent // Cast to Founder
                  }.toList
                } catch {
                  case e: Exception =>
                    println(s"[DEBUG] Exception processing founders: ${e.getMessage}")
                    List.empty
                }

                // Extract and cast bank to Bank type
                val bankData = try {
                  val result = bankInfo.flatMap(info => {
                    info.agent match {
                      case agent: Bank =>
                        Some(agent)  // Cast to Bank
                      case other =>
                        None
                    }
                  })
                  result
                } catch {
                  case e: Exception =>
                    println(s"[DEBUG] Exception processing bank: ${e.getMessage}")
                    None
                }

                // Extract and cast market to Market type
                val marketData = try {
                  val result = marketInfo.flatMap(info => {
                    info.agent match {
                      case agent: Market =>
                        Some(agent)  // Cast to Market
                      case other =>
                        println(s"[DEBUG] Failed to cast market agent: ${other.getClass.getName}")
                        None
                    }
                  })
                  result
                } catch {
                  case e: Exception =>
                    println(s"[DEBUG] Exception processing market: ${e.getMessage}")
                    None
                }

                try {
                  val response = FullInfoResponse(region, producersData, foundersData, bankData, marketData)
                  replyTo ! Some(response)
                } catch {
                  case e: Exception =>
                    println(s"[DEBUG] Error creating or sending FullInfoResponse: ${e.getMessage}")
                    println(s"[DEBUG] Sending simplified response")
                    replyTo ! Some(FullInfoResponse(region, List.empty, List.empty, None, None))
                }

              case Failure(exception) =>
                println(s"[DEBUG] Combined future failed: ${exception.getMessage}")
                println(s"[DEBUG] Exception class: ${exception.getClass.getName}")
                println(s"[DEBUG] Exception cause: ${if (exception.getCause != null) exception.getCause.getMessage else "none"}")
                println(s"[DEBUG] Sending simplified response due to failure")
                replyTo ! Some(FullInfoResponse(region, List.empty, List.empty, None, None))
            }

            Behaviors.same

          case MakeBid(sendTo, resourceType, quantity, price) =>
            sendTo ! ReceiveBid(context.self, region.id, resourceType, quantity, price)
            Behaviors.same

          case MakeAsk(sendTo, resourceType, quantity, price) =>
            sendTo ! ReceiveAsk(context.self, region.id, resourceType, quantity, price)
            Behaviors.same

          case BuyFromSeller(seller, resourceType, quantity, price) =>
            seller ! SellToBuyer(context.self, resourceType, quantity, price)
            val updatedResources = region.storedResources + (resourceType -> (region.storedResources.getOrElse(resourceType, 0) + quantity), Money -> (region.storedResources.getOrElse(Money, 0) - Math.multiplyExact(quantity, price)))

            // Log market transaction (buying)
            logEvent(
              EventType.MarketTransaction,
              s"Bought ${quantity} ${resourceType} at price ${price} (Total: ${quantity * price})"
            )

            tick(state.copy(region = region.copy(storedResources = updatedResources)))

          case SellToBuyer(buyer, resourceType, quantity, price) =>
            val updatedResources = region.storedResources +
              (resourceType -> (region.storedResources.getOrElse(resourceType, 0) - quantity),
                Money -> (region.storedResources.getOrElse(Money, 0) + Math.multiplyExact(quantity, price)))

            // Log market transaction (selling)
            logEvent(
              EventType.MarketTransaction,
              s"Sold ${quantity} ${resourceType} at price ${price} (Total: ${quantity * price})"
            )

            tick(state.copy(region = region.copy(storedResources = updatedResources)))


          case ReceiveWorkerBid(replyTo, agentId, price) =>
            if (region.population - region.assignedWorkers > 0) {
              econAgentIds.get("market").flatMap(_.headOption).foreach { marketId =>
                econActors.get(marketId) match {
                  case Some(marketActor: ActorRef[MarketActor.Command]) =>
                    context.ask(marketActor, GetAskPrice(_, Food)) {
                      case Success(Some(foodPrice: Int)) =>
                        if (price >= foodPrice) {
                          replyTo ! Right(())

                          // Log worker hired event
                          logEvent(
                            EventType.WorkerHired,
                            s"Worker hired by ${agentId} at wage ${price}"
                          )

                          AssignWorkers(1)
                        } else {
                          replyTo ! Left(Some(foodPrice))
                          ActorNoOp()
                        }
                      case _ =>
                        ActorNoOp()
                    }
                  case _ =>
                    replyTo ! Left(None)
                }
              }
            } else {
              replyTo ! Left(None)
            }
            Behaviors.same

          case ChangeSeason() =>
            val newSeason = region.season.next

            // Log season change
            logEvent(
              EventType.SeasonChanged,
              s"Season changed from ${region.season} to ${newSeason}"
            )

            tick(state.copy(region = region.copy(season = newSeason)))

          case BuildBank() =>
            econAgentIds.getOrElse("bank", List()).headOption match {
              case None =>
                val bank = Bank.newBank(region.id)
                val actorRef = context.spawn(BankActor(BankActorState(bank, context.self, Map())), bank.id)
                val newAgentEntry = bank.id :: List()
                val newAgentsMap = econAgentIds + ("bank" -> newAgentEntry)
                val newActorsMap = econActors + (bank.id -> actorRef)

                // Log bank construction
                logEvent(
                  EventType.ConstructionCompleted,
                  s"Bank constructed (ID: ${bank.id})"
                )

                tick(state.copy(econAgentIds = newAgentsMap, econActors = newActorsMap))
              case _ =>
                Behaviors.same
            }


          case BuildMarket() =>
            econAgentIds.getOrElse("market", List()).headOption match {
              case None =>
                val market = Market.newMarket(region.id)
                val actorRef = context.spawn(MarketActor(MarketActorState(market, context.self, Map())), market.id)
                val newAgentEntry = market.id :: List()
                val newAgentsMap = econAgentIds + ("market" -> newAgentEntry)
                val newActorsMap = econActors + (market.id -> actorRef)

                // Log market construction
                logEvent(
                  EventType.ConstructionCompleted,
                  s"Market constructed (ID: ${market.id})"
                )

                tick(state.copy(econAgentIds = newAgentsMap, econActors = newActorsMap))
              case _ =>
                Behaviors.same
            }

          case BuildProducer(producer) =>
            val actorRef = context.spawn(ProducerActor(ProducerActorState(producer, Map(), context.self, Map())), producer.id)

            // Get the founder ID from the bonds (if any) for cleanup
            val founderId = if (producer.outstandingBonds.nonEmpty) {
              // Just get the debtorId from the first bond (they should all have the same debtorId)
              producer.outstandingBonds.values.headOption.map(_.debtorId)
            } else {
              None
            }

            // Update the bank's records if there are outstanding bonds
            if (producer.outstandingBonds.nonEmpty) {
              econAgentIds.get("bank").flatMap(_.headOption).foreach { bankId =>
                econActors.get(bankId).foreach {
                  case bankActor: ActorRef[BankActor.Command] =>
                    // Notify bank of the new debtor ID for these bonds with explicit actor reference
                    bankActor ! UpdateBondDebtor(producer.outstandingBonds.keys.toList, producer.id, actorRef)
                  case _ =>
                  // Do nothing if not a BankActor
                }
              }
            }

            // Add the new producer
            val updatedProducerIds = producer.id :: econAgentIds.getOrElse("producers", List())
            val updatedAgentIds = econAgentIds + ("producers" -> updatedProducerIds)
            val updatedActors = econActors + (producer.id -> actorRef)

            // Log producer construction
            logEvent(
              EventType.ConstructionCompleted,
              s"Producer constructed (ID: ${producer.id}, Type: ${producer.resourceProduced})"
            )

            // Remove the founder from econAgentIds and econActors if we found a founderId
            val (finalAgentIds, finalActors) = founderId match {
              case Some(id) =>
                // Remove founder from econAgentIds (filter it out of the "founders" list)
                val cleanedAgentIds = updatedAgentIds.updated(
                  "founders",
                  updatedAgentIds.getOrElse("founders", List()).filterNot(_ == id)
                )

                // Remove founder from econActors
                val cleanedActors = updatedActors - id

                (cleanedAgentIds, cleanedActors)

              case None =>
                // No founder to remove
                (updatedAgentIds, updatedActors)
            }

            tick(state.copy(econAgentIds = finalAgentIds, econActors = finalActors))

          case SpawnFounder() =>
            // Heuristic for limiting amount of founders happening at once
            if (econAgentIds.getOrElse("founders", List()).length < 3) {
              val newId = UUID.randomUUID().toString
              val founder = Founder(newId, region.id, None)
              val actorRef = context.spawn(FounderActor(FounderActorState(founder, context.self)), newId)
              val updatedFounderIds = founder.id :: econAgentIds.getOrElse("founders", List())
              val updatedAgentIds = econAgentIds + ("founders" -> updatedFounderIds)
              val updatedActors = econActors + (founder.id -> actorRef)

              // Log founder creation
              logEvent(
                EventType.FounderCreated,
                s"New founder created (ID: ${founder.id})"
              )

              tick(state.copy(econAgentIds = updatedAgentIds, econActors = updatedActors))
            } else {
              Behaviors.same
            }

          case UnassignWorkers(qty) =>
            // Log worker unassignment
            if (qty > 0) {
              logEvent(
                EventType.WorkerHired,
                s"${qty} workers unassigned (Total assigned: ${region.assignedWorkers - qty})"
              )
            }

            tick(state.copy(region = region.copy(assignedWorkers = region.assignedWorkers - qty)))

          case AssignWorkers(qty) =>
            // Log worker assignment
            if (qty > 0) {
              logEvent(
                EventType.WorkerHired,
                s"${qty} workers assigned (Total assigned: ${region.assignedWorkers + qty})"
              )
            }

            tick(state.copy(region = region.copy(assignedWorkers = region.assignedWorkers + qty)))

          case ActorNoOp() =>
            Behaviors.same


          case _ =>
            Behaviors.same
        }
      }
    }

    Behaviors.withTimers { timers =>
      timers.startTimerWithFixedDelay("consumption", ConsumeFood(), 10.second)
      timers.startTimerWithFixedDelay(key = "production", ProduceBaseResources(), 8.second)
      timers.startTimerWithFixedDelay("populationChange", ChangePopulation(), 30.second)
      timers.startTimerWithFixedDelay("seasonChange", ChangeSeason(), 40.second)

      // Spawn the initial founder
      timers.startTimerWithFixedDelay("spawn_founder", SpawnFounder(), 20.second)

      // Spawn the market and thee bank
      timers.startSingleTimer("spawn_market", BuildMarket(), 1.second)
      timers.startSingleTimer("spawn_bank", BuildBank(), 1.second)


      tick(state)
    }
  }
}