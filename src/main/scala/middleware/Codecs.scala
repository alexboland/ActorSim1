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
    case farm: Farm => farm.asJson
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

  implicit val farmEncoder: Encoder[Farm] = ResourceProducerEncoderHelper.combineEncoders({ farm =>
    Json.obj(
      "storedFood" -> Json.fromInt(farm.storedFood),
      "type" -> Json.fromString("Farm")
    )
  })

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