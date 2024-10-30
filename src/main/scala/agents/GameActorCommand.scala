package agents

import akka.actor.typed.ActorRef
import middleware.GameInfo

trait GameActorCommand

trait GameAgent {
  val id: String
}

trait EconAgent extends GameAgent

trait ResourceProducer extends EconAgent {
  val workers: Int
  val maxWorkers: Int
  val resourceProduced: ResourceType
  val inputs: Map[ResourceType, Int] //resources needed as inputs to the production process
  val multipliers: Map[ResourceType, Int] //resources that multiply output by a certain amount
  val baseProduction: Int
}

trait EconActorCommand extends GameActorCommand

trait ResourceProducerCommand extends EconActorCommand

case class ActorNoOp() extends GameActorCommand with RegionActor.Command with ResourceProducerCommand with BankActor.Command

case class MakeWorkerBid(sendTo: ActorRef[RegionActor.Command], wage: Int)
  extends ResourceProducerCommand

case class AcceptWorkerBid() extends ResourceProducerCommand

case class RejectWorkerBid() extends ResourceProducerCommand

case class PayWages() extends ResourceProducerCommand

case class AddWorker() extends ResourceProducerCommand

case class ProduceResource() extends ResourceProducerCommand

case class AcceptBid() extends EconActorCommand

case class RejectBid() extends EconActorCommand

case class IssueBond(principal: Int, interestRate: Double, issueTo: String)
  extends EconActorCommand with ResourceProducerCommand with BankActor.Command with RegionActor.Command

case class ReceiveBond(bond: Bond, replyTo: ActorRef[Boolean], issuedFrom: ActorRef[ResourceProducerCommand])
  extends GovernmentActor.Command with BankActor.Command

case class AddOutstandingBond(bond: Bond, issuedTo: String) extends ResourceProducerCommand

case class PayBond(bond: Bond, amount: Int, replyTo: ActorRef[Int]) extends ResourceProducerCommand

case class MakeBid(sendTo: ActorRef[EconActorCommand], resourceType: ResourceType, quantity: Int, price: Int)
  extends EconActorCommand with ResourceProducerCommand with RegionActor.Command with GovernmentActor.Command

case class ReceiveBid(replyTo: ActorRef[EconActorCommand], resourceType: ResourceType, quantity: Int, price: Int)
  extends EconActorCommand with ResourceProducerCommand with GovernmentActor.Command

case class ShowInfo(replyTo: ActorRef[Option[GameInfo.InfoResponse]])
  extends GameActorCommand with RegionActor.Command with ResourceProducerCommand with GovernmentActor.Command with BankActor.Command

case class GetBidPrice(replyTo: ActorRef[Option[Int]], resourceType: ResourceType)
  extends EconActorCommand with ResourceProducerCommand with GovernmentActor.Command

case class GetAskPrice(replyTo: ActorRef[Option[Int]], resourceType: ResourceType)
  extends EconActorCommand with ResourceProducerCommand with GovernmentActor.Command
