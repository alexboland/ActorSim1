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

  case class CounterOffer(qty: Int, price: Int)
}

type EconActor = ActorRef[BankActor.Command] | ActorRef[GovernmentActor.Command]
  | ActorRef[RegionActor.Command] | ActorRef[MarketActor.Command] | ActorRef[ProducerActor.Command]
  | ActorRef[FounderActor.Command] | ActorRef[BankActor.Command] // Expand on this as necessary

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

// TODO refactor bidding messages in following possible ways:
// (1) create Bid/Ask case class with quantity and price fields
// (2) change replyTo to accept an Either instead of an EconAgent.Command
// partially hesitating on (2) just in case I decide to stop using the ask pattern for bidding
case class AcceptBid() extends EconAgent.Command

case class RejectBid(coOpt: Option[EconAgent.CounterOffer]) extends EconAgent.Command

case class AcceptAsk() extends EconAgent.Command

case class RejectAsk(coOpt: Option[EconAgent.CounterOffer]) extends EconAgent.Command

case class IssueBond(sendTo: ActorRef[BankingCommand], principal: Int, interestRate: Double)
  extends EconAgent.Command

case class ReceiveBond(bond: Bond, replyTo: ActorRef[Option[Bond]], issuedFrom: EconActor)
  extends BankingCommand // for now, using the Bond the "counteroffer", with an identical bond being acceptance and None being no dice

case class AddOutstandingBond(bond: Bond) extends EconAgent.Command

case class PayBond(bond: Bond, amount: Int, replyTo: ActorRef[Int]) extends EconAgent.Command

case class MakeBid(sendTo: EconActor, resourceType: ResourceType, quantity: Int, price: Int)
  extends EconAgent.Command

case class BuyFromSeller(seller: EconActor, resourceType: ResourceType, quantity: Int, price: Int) extends EconAgent.Command

case class SellToBuyer(seller: EconActor, resourceType: ResourceType, quantity: Int, price: Int) extends EconAgent.Command

case class ReceiveBid(replyTo: EconActor, bidderId: String,  resourceType: ResourceType, quantity: Int, price: Int)
  extends EconAgent.Command

case class MakeAsk(sendTo: EconActor, resourceType: ResourceType, quantity: Int, price: Int) extends EconAgent.Command

case class ReceiveAsk(sendTo: EconActor, askerId: String, resourceType: ResourceType, quantity: Int, price: Int) extends EconAgent.Command

case class ShowInfo(replyTo: ActorRef[Option[GameInfo.InfoResponse]])
  extends GameActorCommand

case class GetBidPrice(replyTo: ActorRef[Option[Int]], resourceType: ResourceType)
  extends EconAgent.Command with ResourceProducer.Command with GovernmentActor.GovtCommand

case class GetAskPrice(replyTo: ActorRef[Option[Int]], resourceType: ResourceType)
  extends EconAgent.Command with ResourceProducer.Command with GovernmentActor.GovtCommand

case class PurchaseResource(resourceType: ResourceType, quantity: Int, price: Int) extends EconAgent.Command

case class ReceiveSalePayment(amount: Int) extends EconAgent.Command
