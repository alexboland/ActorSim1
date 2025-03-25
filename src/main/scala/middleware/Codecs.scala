package middleware

import agents.Region.Season
import agents._
import cats.syntax.either._
import io.circe._
import io.circe.syntax._

// JSON encoders and decoders
object JsonCodecs {


  implicit val seasonEncoder: Encoder[Season] = Encoder.encodeString.contramap[Season](_.toString)
  implicit val seasonDecoder: Decoder[Season] = Decoder.decodeString.emap {
    case "Spring" => Right(Region.Spring)
    case "Summer" => Right(Region.Summer)
    case "Autumn" => Right(Region.Autumn)
    case "Winter" => Right(Region.Winter)
    case s => Left(s"Invalid season: $s")
  }

  implicit val resourceTypeEncoder: Encoder[ResourceType] = Encoder.encodeString.contramap[ResourceType](_.toString)
  implicit val resourceTypeDecoder: Decoder[ResourceType] = Decoder.decodeString.emap {
    case "Wood" => Right(Wood)
    case "Food" => Right(Food)
    case "Copper" => Right(Copper)
    case s => Left(s"Invalid resource type: $s")
  }

  implicit val resourceTypeKeyEncoder: KeyEncoder[ResourceType] = new KeyEncoder[ResourceType] {
    override def apply(key: ResourceType): String = key.toString
  }
  implicit val resourceTypeKeyDecoder: KeyDecoder[ResourceType] = new KeyDecoder[ResourceType] {
    override def apply(key: String): Option[ResourceType] = key match {
      case "Wood" => Some(Wood)
      case "Food" => Some(Food)
      case _ => None
    }
  }

  implicit val bondEncoder: Encoder[Bond] = new Encoder[Bond] {
    override def apply(bond: Bond): Json = Json.obj(
      "id" -> bond.id.asJson,
      "principal" -> bond.principal.asJson,
      "interestRate" -> bond.interestRate.asJson,
      "totalOutstanding" -> bond.totalOutstanding.asJson,
      "debtorId" -> bond.debtorId.asJson
    )
  }

  implicit val regionLocationEncoder: Encoder[Region.Location] = new Encoder[Region.Location] {
    override def apply(location: Region.Location): Json = Json.obj(
      "x" -> Json.fromInt(location.x),
      "y" -> Json.fromInt(location.y)
    )
  }

  // Manual encoder for Region
  implicit val regionEncoder: Encoder[Region] = new Encoder[Region] {
    final def apply(region: Region): Json = Json.obj(
      "id" -> region.id.asJson,
      "storedResources" -> region.storedResources.asJson,
      "population" -> region.population.asJson,
      "baseProduction" -> region.baseProduction.asJson,
      "season" -> region.season.asJson,
      "location" -> region.location.asJson
    )
  }

  implicit val producerEncoder: Encoder[Producer] = new Encoder[Producer] {
    final def apply(producer: Producer): Json = Json.obj(
      "id" -> producer.id.asJson,
      "regionId" -> producer.regionId.asJson,
      "workers" -> producer.workers.asJson,
      "resourceProduced" -> producer.resourceProduced.asJson,
      "storedResources" -> producer.storedResources.asJson,
      "wage" -> producer.wage.asJson,
      "maxWorkers" -> producer.maxWorkers.asJson,
      "baseProduction" -> producer.baseProduction.asJson,
      "inputs" -> producer.inputs.asJson,
      "multipliers" -> producer.multipliers.asJson,
      "outstandingBonds" -> producer.outstandingBonds.asJson
    )
  }

  implicit val marketEncoder: Encoder[Market] = new Encoder[Market] {
    final def apply(market: Market): Json = {

      val topBids = market.bids.map { case (resourceType, bids) =>
        resourceType -> bids.headOption.map(_.price)
      }
      val topAsks = market.asks.map { case (resourceType, asks) =>
        resourceType -> asks.headOption.map(_.price)
      }

      Json.obj(
        "id" -> market.id.asJson,
        "regionId" -> market.regionId.asJson,
        "localId" -> market.localId.asJson,
        "topBids" -> topBids.asJson,
        "topAsks" -> topAsks.asJson
      )
    }
  }

  implicit val bankEncoder: Encoder[Bank] = new Encoder[Bank] {
    final def apply(bank: Bank): Json = Json.obj(
      "id" -> bank.id.asJson,
      "regionId" -> bank.regionId.asJson,
      "storedMoney" -> bank.storedMoney.asJson,
      "interestRate" -> bank.interestRate.asJson,
      "bondsOwned" -> bank.bondsOwned.asJson,
      "outstandingBonds" -> bank.outstandingBonds.asJson
    )
  }

  implicit val constructionSiteEncoder: Encoder[ConstructionSite] = new Encoder[ConstructionSite] {
    final def apply(site: ConstructionSite): Json = Json.obj(
      "id" -> site.id.asJson,
      "facility" -> site.facility.asJson,
      "percentComplete" -> site.percentComplete.asJson,
    )
  }

  implicit val founderEncoder: Encoder[Founder] = new Encoder[Founder] {
    final def apply(founder: Founder): Json = Json.obj(
      "id" -> founder.id.asJson,
      "regionId" -> founder.regionId.asJson,
      "site" -> founder.site.asJson
    )
  }

  // Manual encoder for Government
  implicit val governmentEncoder: Encoder[Government] = new Encoder[Government] {
    final def apply(government: Government): Json = Json.obj(
      "storedResources" -> government.storedResources.asJson,
      "askPrices" -> government.askPrices.asJson,
      "bidPrices" -> government.bidPrices.asJson,
      "regions" -> government.regions.keys.asJson
    )
  }

  implicit val gameAgentEncoder: Encoder[GameAgent] = Encoder.instance {
    case region: Region => region.asJson
    case _ => Json.obj()
  }

  object ResourceProducerEncoderHelper {
    def encodeCommon(rp: ResourceProducer): Json = Json.obj(
      "workers" -> Json.fromInt(rp.workers),
      "maxWorkers" -> Json.fromInt(rp.maxWorkers),
      "resourceProduced" -> rp.resourceProduced.asJson,
      "inputs" -> rp.inputs.asJson,
      "multipliers" -> rp.multipliers.asJson,
      "baseProduction" -> Json.fromInt(rp.baseProduction)
    )

    def combineEncoders[A <: ResourceProducer](specific: A => Json): Encoder[A] =
      (a: A) => ResourceProducerEncoderHelper.encodeCommon(a).deepMerge(specific(a))
  }

  // Manual decoder for Region
  /*implicit val regionDecoder: Decoder[Region] = new Decoder[Region] {
    final def apply(c: HCursor): Decoder.Result[Region] =
      for {
        id <- c.downField("id").as[String]
        laborAssignments <- c.downField("laborAssignments").as[Map[String, Int]]
        storedResources <- c.downField("storedResources").as[Map[ResourceType, Int]]
        population <- c.downField("population").as[Int]
        baseProduction <- c.downField("baseProduction").as[Map[ResourceType, Int]]
        season <- c.downField("season").as[Season]
        government <- c.downField("government").as[String]
      } yield Region(Map[ActorRef[EconActorCommand], Int](), storedResources, population, baseProduction, season, Government())
  }*/
}