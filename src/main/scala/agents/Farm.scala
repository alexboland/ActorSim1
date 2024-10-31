package agents

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout
import middleware.GameInfo

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

case class FarmActorState(
                             farm: Farm,
                             econActors: Map[String, ActorRef[BankActor.Command]],
                           )

case class Farm(
  id: String,
  workers: Int,
  storedFood: Int,
  wage: Int,
  storedMoney: Int,
  maxWorkers: Int,
  baseProduction: Int,
  inputs: Map[ResourceType, Int],
  multipliers: Map[ResourceType, Int],
  outstandingBonds: Map[String, Bond]
) extends ResourceProducer {
  override val resourceProduced: ResourceType = Food
}

object Farm {
  def newFarm(baseProduction: Int): Farm = {
    Farm(
      id = UUID.randomUUID().toString,
      workers = 0,
      storedFood = 0,
      wage = 0,
      storedMoney = 0,
      maxWorkers = 10,
      baseProduction = baseProduction,
      inputs = Map(),
      multipliers = Map(),
      outstandingBonds = Map())
  }
}

object FarmActor {

  case class InfoResponse(farm: Farm) extends GameInfo.InfoResponse {
    override val agent: Farm = farm
  }
  
  type Command = ResourceProducer.Command | GameActorCommand

  def apply(state: FarmActorState): Behavior[Command] = Behaviors.setup { context =>
    implicit val timeout: Timeout = Timeout(3.seconds) // Define an implicit timeout for ask pattern
    def tick(state: FarmActorState): Behavior[Command] = {
      val farm = state.farm
      Behaviors.receive { (context, message) =>
        message match {
          case ReceiveBid(replyTo, resourceType, quantity, price) =>
            if (resourceType != Food) {
              replyTo ! RejectBid()
              tick(state.copy(farm = farm))
            } else if (farm.storedFood < quantity) {
              replyTo ! RejectBid()
              tick(state.copy(farm = farm)) // Insufficient resources to sell, do nothing
              // TODO create some kind of threshold to dictate how much capacity it wants to retain
            } else {
              // Currently dumb behavior where it doesn't negotiate
              replyTo ! AcceptBid()
              tick(state.copy(
                farm = farm.copy(storedFood = farm.storedFood - quantity, storedMoney = farm.storedMoney + Math.multiplyExact(quantity, price))))
            }

          case PayWages() =>
            val totalPayment = Math.multiplyExact(farm.workers, farm.wage)
            if (totalPayment > farm.storedMoney) {
              // Take out loan from bank or lose workers or something, still working on it
            }
            tick(state.copy(farm = farm.copy(storedMoney = farm.storedMoney - totalPayment)))

          case ProduceResource() =>
            tick(state.copy(
              farm = farm.copy(storedFood = farm.storedFood + Math.multiplyExact(farm.baseProduction, farm.workers))))

          case ShowInfo(replyTo) =>
            replyTo ! Some(InfoResponse(farm))
            Behaviors.same

          case MakeWorkerBid(sendTo, wage) =>
            context.ask(sendTo, RegionActor.ReceiveWorkerBid(_, state.farm.id, wage)) {
              case Success(true) =>
                AddWorker()
              case _ =>
                ActorNoOp()

            }
            Behaviors.same

          case AddWorker() =>
            tick(state = state.copy(farm = farm.copy(workers = farm.workers + 1)))

          case IssueBond(principal, interest, issueTo) =>
             state.econActors.get(issueTo) match {
                case Some(actorRef) =>
                  val bond = Bond(UUID.randomUUID().toString, principal, interest, principal, issueTo, farm.id)
                  context.ask(actorRef, ReceiveBond(bond, _, context.self)) {
                    case Success(true) =>
                      AddOutstandingBond(bond, issueTo)
                    case Success(false) =>
                      ActorNoOp()
                    case Failure(err) =>
                      println(s"failure in command IssueBond in farm ${farm.id}: ${err}")
                      ActorNoOp()
                    case _ =>
                      ActorNoOp()
                  }
                case _ =>
                  println(s"couldn't find actor ref for bank ${issueTo}")
              }
            Behaviors.same

          case AddOutstandingBond(bond, issuedTo) =>
            val newStoredMoney = state.farm.storedMoney + bond.principal
            val newOutstandingBonds = state.farm.outstandingBonds + (bond.id -> bond)

            tick(state = state.copy(farm = state.farm.copy(storedMoney = newStoredMoney, outstandingBonds = newOutstandingBonds)))

          case PayBond(bond, amount, replyTo) =>
            val amountToPay = Math.min(state.farm.storedMoney, amount) // For now just have it pay what it can without defaults
            val updatedBond = bond.copy(totalOutstanding = Math.round((bond.totalOutstanding - amountToPay)*bond.interestRate).toInt)
            val newOutstandingBonds = if (updatedBond.totalOutstanding <= 0) {
              state.farm.outstandingBonds - bond.id
            } else {
              state.farm.outstandingBonds + (bond.id -> updatedBond)
            }
            replyTo ! amountToPay
            tick(state = state.copy(farm = farm.copy(storedMoney = state.farm.storedMoney - amountToPay, outstandingBonds = newOutstandingBonds)))


          case _ =>
            Behaviors.same
        }
      }
    }

    Behaviors.withTimers { timers =>
      timers.startTimerWithFixedDelay("production", ProduceResource(), 8.second)

      tick(state)
    }
  }
}