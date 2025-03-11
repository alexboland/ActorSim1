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
    def tick(state: RegionActorState): Behavior[Command] = {

      implicit val scheduler = context.system.scheduler
      implicit val ec = context.executionContext

      Behaviors.receive { (context, message) =>
        val region = state.region
        val econAgentIds = state.econAgentIds
        val econActors = state.econActors
        message match {
          // For encapsulation, bids/asks from producers will be mediated through region agent into the market
          case ReceiveBid(replyTo, bidderId, resourceType, quantity, price) =>
            econAgentIds.getOrElse("market", List()).headOption.map { marketId =>
              if (bidderId != marketId) {
                econActors.get(marketId).map { marketActor =>
                  marketActor ! ReceiveBid(replyTo, bidderId, resourceType, quantity, price)
                }
              } else {
                // If the market is "making a bid" it's to forward unmet demand to founders
                econAgentIds.get("founders").map(_.map { founderId =>
                  econActors.get(founderId).map { founderActor =>
                    founderActor ! ReceiveBid(replyTo, bidderId, resourceType, quantity, price)
                  }
                })
              }
              Behaviors.same
            }.getOrElse(Behaviors.same)

          case ReceiveAsk(replyTo, askerId, resourceType, quantity, price) =>
            econAgentIds.getOrElse("market", List()).headOption.map { marketId =>
              if (askerId != marketId) {
                econActors.get(marketId).foreach { marketActor =>
                  marketActor ! ReceiveAsk(replyTo, askerId, resourceType, quantity, price)
                }
              }
              Behaviors.same
            }.getOrElse(Behaviors.same)

          case GetBidPrice(replyTo, resourceType) =>
            econAgentIds.get("market").flatMap(_.headOption).map { marketId =>
              econActors.get(marketId)
                .collect { case (actor: ActorRef[MarketActor.Command]) => actor }.map { marketActor =>
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
            econAgentIds.get("bank").flatMap(_.headOption).map { bankId =>
                if (bond.debtorId == bankId) {
                  econAgentIds.getOrElse("government", List()).headOption.foreach { id =>
                    econActors.get(id) match {
                      case govtActor: ActorRef[GovernmentActor.Command] =>
                        govtActor ! ReceiveBond(bond, replyTo, issuedFrom)
                    }
                  }
                } else {
                  econActors.get(bankId).match {
                    case bankActor: ActorRef[BankActor.Command] =>
                      bankActor ! ReceiveBond(bond, replyTo, issuedFrom)
                  }
                }
                Behaviors.same
            }.getOrElse(Behaviors.same)

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

            val newPop = if (foodConsumed > region.storedResources(Food)) {
              Math.max(region.population - Math.round((foodConsumed - region.storedResources(Food))/5), 0)
            } else {
              region.population
            }

            if (updatedResources(Food) < region.population * 1.5) {
              econAgentIds.getOrElse("market", List()).headOption.foreach { id =>
                econActors.get(id).foreach { marketActor =>
                  context.ask(marketActor, GetAskPrice(_, Food)) {
                    case Success(Some(price: Int)) =>
                      val qtyToBuy = Math.min(updatedResources.getOrElse(Money, 0) / price, Math.round(region.population * 1.5f) - updatedResources(Food))
                      // TODO replace the below message with making a bid to the market
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

            /*println("================================")
            println(s"population: $region.population")
            println(s"growth factor: $growthFactor")
            println(s"new population: $newPop")
            println("================================")*/

            tick(state.copy(region = region.copy(population = newPop)))

          case ShowInfo(replyTo) =>
            replyTo ! Some(InfoResponse(region))
            Behaviors.same

          case ShowFullInfo(replyTo) =>
            //println(s"[DEBUG] ShowFullInfo received for region ${region.id}")

            // Get producers list
            //println(s"[DEBUG] Found ${econActors.getOrElse("producers", List.empty).size} producers")
            val producersFutures = econAgentIds.getOrElse("producers", List())
              .flatMap(id => econActors.get(id).map(actor => actor.ask(ShowInfo.apply)))

            // Get founders list
            //println(s"[DEBUG] Found ${econActors.getOrElse("founders", List.empty).size} founders")
            val foundersFutures = econAgentIds.getOrElse("founders", List())
              .flatMap(id => econActors.get(id).map(actor => actor.ask(ShowInfo.apply)))

            // Get the first bank
            //println(s"[DEBUG] Bank exists: ${econActors.getOrElse("bank", List.empty).headOption.isDefined}")
            val bankFuture = econAgentIds.getOrElse("bank", List()).headOption.flatMap(id => {
              //println(s"[DEBUG] Asking bank ${actor.path.name} for info")
              econActors.get(id).map { actor =>
                actor.ask(ShowInfo.apply)
              }
            }).getOrElse({
              //println("[DEBUG] No bank found, using empty future")
              Future.successful(None)
            })

            // Get the first market
            //println(s"[DEBUG] Market exists: ${econActors.getOrElse("market", List.empty).headOption.isDefined}")
            val marketFuture = econAgentIds.getOrElse("market", List()).headOption.flatMap(id => {
              //println(s"[DEBUG] Asking market ${actor.path.name} for info")
              econActors.get(id).map { actor =>
                actor.ask(ShowInfo.apply)
              }
            }).getOrElse({
              //println("[DEBUG] No market found, using empty future")
              Future.successful(None)
            })

            // Combine all futures
            //println("[DEBUG] Starting to combine futures")
            val producersAggregate = Future.sequence(producersFutures)
            val foundersAggregate = Future.sequence(foundersFutures)

            //println("[DEBUG] Creating combined future with for-comprehension")
            val combinedFuture = for {
              producers <- producersAggregate
              //_ = println(s"[DEBUG] Received responses from ${producers.size} producers")
              founders <- foundersAggregate
              //_ = println(s"[DEBUG] Received responses from ${founders.size} founders")
              bank <- bankFuture
              //_ = println(s"[DEBUG] Received bank response: ${bank.isDefined}")
              market <- marketFuture
              //_ = println(s"[DEBUG] Received market response: ${market.isDefined}")
            } yield (producers, founders, bank, market)

            //println("[DEBUG] Setting up onComplete handler for combined future")
            combinedFuture.onComplete {
              case Success((producersInfo, foundersInfo, bankInfo, marketInfo)) =>
                //println("[DEBUG] Combined future completed successfully")

                // Extract and cast producers to Producer type
                //println(s"[DEBUG] Processing ${producersInfo.size} producer responses")
                val producersData = try {
                  producersInfo.flatMap(info => {
                    //println(s"[DEBUG] Processing producer info: ${info}")
                    info.map(infoResponse => {
                      //println(s"[DEBUG] Processing producer agent: ${infoResponse.agent.getClass.getName}")
                      infoResponse.agent
                    })
                  }).collect {
                    case agent: Producer =>
                      //println(s"[DEBUG] Successfully cast producer agent: ${agent.id}")
                      agent  // Cast to Producer
                  }.toList
                  //println(s"[DEBUG] Extracted ${result.size} valid producers")
                } catch {
                  case e: Exception =>
                    //println(s"[DEBUG] Exception processing producers: ${e.getMessage}")
                    List.empty
                }

                // Extract and cast founders to Founder type
                //println(s"[DEBUG] Processing ${foundersInfo.size} founder responses")
                val foundersData = try {
                  foundersInfo.flatMap(info => {
                    //println(s"[DEBUG] Processing founder info: ${info}")
                    info.map(infoResponse => {
                      //println(s"[DEBUG] Processing founder agent: ${infoResponse.agent.getClass.getName}")
                      infoResponse.agent
                    })
                  }).collect {
                    case agent: Founder =>
                      //println(s"[DEBUG] Successfully cast founder agent: ${agent.id}")
                      agent // Cast to Founder
                  }.toList
                  //println(s"[DEBUG] Extracted ${result.size} valid founders")
                } catch {
                  case e: Exception =>
                    //println(s"[DEBUG] Exception processing founders: ${e.getMessage}")
                    List.empty
                }

                // Extract and cast bank to Bank type
                //println(s"[DEBUG] Processing bank response: ${bankInfo}")
                val bankData = try {
                  val result = bankInfo.flatMap(info => {
                    //println(s"[DEBUG] Processing bank info agent: ${if (info != null) info.agent.getClass.getName else "null"}")
                    info.agent match {
                      case agent: Bank =>
                        //println(s"[DEBUG] Successfully cast bank agent: ${agent.id}")
                        Some(agent)  // Cast to Bank
                      case other =>
                        //println(s"[DEBUG] Failed to cast bank agent: ${other.getClass.getName}")
                        None
                    }
                  })
                  //println(s"[DEBUG] Bank data extracted: ${result.isDefined}")
                  result
                } catch {
                  case e: Exception =>
                    //println(s"[DEBUG] Exception processing bank: ${e.getMessage}")
                    None
                }

                // Extract and cast market to Market type
                //println(s"[DEBUG] Processing market response: ${marketInfo}")
                val marketData = try {
                  val result = marketInfo.flatMap(info => {
                    //println(s"[DEBUG] Processing market info agent: ${if (info != null) info.agent.getClass.getName else "null"}")
                    info.agent match {
                      case agent: Market =>
                        //println(s"[DEBUG] Successfully cast market agent: ${agent.id}")
                        Some(agent)  // Cast to Market
                      case other =>
                        //println(s"[DEBUG] Failed to cast market agent: ${other.getClass.getName}")
                        None
                    }
                  })
                  //println(s"[DEBUG] Market data extracted: ${result.isDefined}")
                  result
                } catch {
                  case e: Exception =>
                    //println(s"[DEBUG] Exception processing market: ${e.getMessage}")
                    None
                }

                //println("[DEBUG] Creating FullInfoResponse")
                try {
                  val response = FullInfoResponse(region, producersData, foundersData, bankData, marketData)
                  //println("[DEBUG] Sending response back to requester")
                  replyTo ! Some(response)
                  //println("[DEBUG] Response sent successfully")
                } catch {
                  case e: Exception =>
                    //println(s"[DEBUG] Error creating or sending FullInfoResponse: ${e.getMessage}")
                    //println(s"[DEBUG] Sending simplified response")
                    replyTo ! Some(FullInfoResponse(region, List.empty, List.empty, None, None))
                }

              case Failure(exception) =>
                //println(s"[DEBUG] Combined future failed: ${exception.getMessage}")
                //println(s"[DEBUG] Exception class: ${exception.getClass.getName}")
                //println(s"[DEBUG] Exception cause: ${if (exception.getCause != null) exception.getCause.getMessage else "none"}")
                //println(s"[DEBUG] Sending simplified response due to failure")
                replyTo ! Some(FullInfoResponse(region, List.empty, List.empty, None, None))
            }

            //println("[DEBUG] ShowFullInfo handler completed, returning Behaviors.same")
            Behaviors.same

          case MakeBid(sendTo, resourceType, quantity, price) =>
            context.ask(sendTo, ReceiveBid(_, region.id, resourceType, quantity, price)) {
              case Success(AcceptBid()) =>
                BuyFromSeller(sendTo, resourceType, quantity, price)
              case Success(RejectBid(Some(co))) =>
                MakeBid(sendTo, resourceType, co.qty, co.price)
              case _ =>
                ActorNoOp()

            }
            Behaviors.same

          case MakeAsk(sendTo, resourceType, quantity, price) =>
            sendTo ! ReceiveAsk(context.self, region.id, resourceType, quantity, price)
            Behaviors.same

          case BuyFromSeller(seller, resourceType, quantity, price) =>
            seller ! SellToBuyer(context.self, resourceType, quantity, price)
            val updatedResources = region.storedResources + (resourceType -> (region.storedResources.getOrElse(resourceType, 0) + quantity), Money -> (region.storedResources.getOrElse(Money, 0) - Math.multiplyExact(quantity, price)))
            tick(state.copy(region = region.copy(storedResources = updatedResources)))

          case SellToBuyer(buyer, resourceType, quantity, price) =>
            val updatedResources = region.storedResources +
              (resourceType -> (region.storedResources.getOrElse(resourceType, 0) - quantity),
                Money -> (region.storedResources.getOrElse(Money, 0) + Math.multiplyExact(quantity, price)))
            tick(state.copy(region = region.copy(storedResources = updatedResources)))


          case ReceiveWorkerBid(replyTo, agentId, price) =>
            if (region.population - region.assignedWorkers > 0) {
              econAgentIds.get("market").flatMap(_.headOption).map { marketId =>
                econActors.get(marketId) match {
                  case Some(marketActor: ActorRef[MarketActor.Command]) =>
                    context.ask(marketActor, GetAskPrice(_, Food)) {
                      case Success(Some(foodPrice: Int)) =>
                        if (price >= foodPrice) {
                          replyTo ! Right(())
                          AssignWorkers(1)
                        } else {
                          replyTo ! Left(Some(foodPrice))
                          ActorNoOp()
                        }
                      case _ =>
                        ActorNoOp()
                    }
                    Behaviors.same
                  case _ =>
                    replyTo ! Left(None)
                    Behaviors.same
                }
              }.getOrElse(Behaviors.same)
            } else {
              replyTo ! Left(None)
              Behaviors.same
            }

          case ChangeSeason() =>
            tick(state.copy(region = region.copy(season = region.season.next)))

          case BuildBank() =>
            econAgentIds.getOrElse("bank", List()).headOption match {
              case None =>
                val bank = Bank.newBank(region.id)
                val actorRef = context.spawn(BankActor(BankActorState(bank, context.self, Map())), bank.id)
                val newAgentEntry = bank.id :: List()
                val newAgentsMap = econAgentIds + ("bank" -> newAgentEntry)
                val newActorsMap = econActors + (bank.id -> actorRef)
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
                tick(state.copy(econAgentIds = newAgentsMap, econActors = newActorsMap))
              case _ =>
                Behaviors.same
            }

          case BuildProducer(producer) =>
            val actorRef = context.spawn(ProducerActor(ProducerActorState(producer, Map(), context.self, Map())), producer.id)
            val updatedProducerIds = producer.id :: econAgentIds.getOrElse("producers", List())
            val updatedAgentIds = econAgentIds + ("producers" -> updatedProducerIds)
            val updatedActors = econActors + (producer.id -> actorRef)
            tick(state.copy(econAgentIds = updatedAgentIds, econActors = updatedActors))

          case SpawnFounder() =>
            // Heuristic for limiting amount of founders happening at once
            // TODO figure out ways to vary the "temperament" of different founders so they all respond to bid signals differently
            // As of right now, this is going to cause all of them to overproduce on the same thing
            if (econAgentIds.getOrElse("founders", List()).length < 3) {
              val newId = UUID.randomUUID().toString
              val founder = Founder(newId, region.id, None)
              val actorRef = context.spawn(FounderActor(FounderActorState(founder, context.self)), newId)
              val updatedFounderIds = founder.id :: econAgentIds.getOrElse("founders", List())
              val updatedAgentIds = econAgentIds + ("founders" -> updatedFounderIds)
              val updatedActors = econActors + (founder.id -> actorRef)
              tick(state.copy(econAgentIds = updatedAgentIds, econActors = updatedActors))
            } else {
              Behaviors.same
            }

          case UnassignWorkers(qty) =>
            tick(state.copy(region = region.copy(assignedWorkers = region.assignedWorkers - qty)))

          case AssignWorkers(qty) =>
            tick(state.copy(region = region.copy(assignedWorkers = region.assignedWorkers + qty)))

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
      timers.startTimerWithFixedDelay("spawn_founder", SpawnFounder(), 20.second)

      // Spawn the market and thee bank
      timers.startSingleTimer("spawn_market", BuildMarket(), 1.second)
      timers.startSingleTimer("spawn_bank", BuildBank(), 1.second)


      tick(state)
    }
  }
}