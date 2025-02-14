package agents

import akka.actor.typed.ActorRef
import middleware.GameInfo

trait GameActorCommand

trait GameAgent {
  val id: String
}

trait EconAgent extends GameAgent {
  val regionId: String
}

object EconAgent {
  trait Command extends GameActorCommand
}

type EconActor = ActorRef[BankActor.Command] | ActorRef[GovernmentActor.Command]
  | ActorRef[RegionActor.Command] | ActorRef[FarmActor.Command]
  | ActorRef[MarketActor.Command] // Expand on this as necessary

trait ResourceProducer extends EconAgent {
  val workers: Int
  val maxWorkers: Int
  val resourceProduced: ResourceType
  val inputs: Map[ResourceType, Int] //resources needed as inputs to the production process
  val multipliers: Map[ResourceType, Int] //resources that multiply output by a certain amount
  val baseProduction: Int
}

object ResourceProducer {
  trait Command
}

trait BankingCommand

case class SetInterestRate(rate: Double) extends BankingCommand

case class ReceiveDeposit(amount: Int) extends BankingCommand

case class CollectBondPayment(bond: Bond, amount: Int) extends BankingCommand

case class DepositBondPayment(bond: Bond, amount: Int) extends BankingCommand

case class ActorNoOp() extends GameActorCommand

case class MakeWorkerBid(sendTo: ActorRef[RegionActor.Command], wage: Int)
  extends ResourceProducer.Command

case class AcceptWorkerBid() extends ResourceProducer.Command

case class RejectWorkerBid() extends ResourceProducer.Command

case class PayWages() extends ResourceProducer.Command

case class AddWorker() extends ResourceProducer.Command

case class ProduceResource() extends ResourceProducer.Command

case class AcceptBid() extends EconAgent.Command

case class RejectBid() extends EconAgent.Command

case class AcceptAsk() extends EconAgent.Command

case class RejectAsk() extends EconAgent.Command

case class IssueBond(principal: Int, interestRate: Double, issueTo: String)
  extends BankingCommand

case class ReceiveBond(bond: Bond, replyTo: ActorRef[Boolean], issuedFrom: EconActor)
  extends BankingCommand

case class AddOutstandingBond(bond: Bond, issuedTo: String) extends EconAgent.Command

case class PayBond(bond: Bond, amount: Int, replyTo: ActorRef[Int]) extends EconAgent.Command

case class MakeBid(sendTo: ActorRef[EconAgent.Command], resourceType: ResourceType, quantity: Int, price: Int)
  extends EconAgent.Command

case class BuyFromSeller(resourceType: ResourceType, quantity: Int, price: Int) extends EconAgent.Command

case class ReceiveBid(replyTo: ActorRef[EconAgent.Command], resourceType: ResourceType, quantity: Int, price: Int)
  extends EconAgent.Command

case class MakeAsk(sendTo: ActorRef[EconAgent.Command], resourceType: ResourceType, quantity: Int, price: Int) extends EconAgent.Command

case class ReceiveAsk(sendTo: ActorRef[EconAgent.Command], resourceType: ResourceType, quantity: Int, price: Int) extends EconAgent.Command

case class ShowInfo(replyTo: ActorRef[Option[GameInfo.InfoResponse]])
  extends GameActorCommand

case class GetBidPrice(replyTo: ActorRef[Option[Int]], resourceType: ResourceType)
  extends EconAgent.Command with ResourceProducer.Command with GovernmentActor.GovtCommand

case class GetAskPrice(replyTo: ActorRef[Option[Int]], resourceType: ResourceType)
  extends EconAgent.Command with ResourceProducer.Command with GovernmentActor.GovtCommand
