trait ResourceType {
  override def toString: String = this.getClass.getSimpleName.dropRight(1)
}

sealed trait Mineable extends ResourceType

case object Food extends ResourceType
case object Wood extends ResourceType
case object Copper extends Mineable
case object Tin extends Mineable
case object Nickel extends Mineable
case object IronOre extends Mineable
case object Coal extends Mineable
case object Money extends ResourceType
case object Limestone extends Mineable

sealed trait TechProduct extends ResourceType {
  val Ingredients: Map[ResourceType, Int]
  val LaborHours: Int
  val Multipliers: Map[ResourceType, Int]
}

case object Bronze extends TechProduct {
  override val Ingredients = Map(Copper -> 1, Tin -> 1)
  override val LaborHours = 4
  override val Multipliers = Map()
}

case object Iron extends TechProduct {
  override val Ingredients = Map(IronOre -> 1, Coal -> 1)
  override val LaborHours = 4
  override val Multipliers = Map()
}

case object CopperHoe extends TechProduct {
  override val Ingredients = Map(Wood -> 1, Copper -> 1)
  override val LaborHours = 2
  override val Multipliers = Map(Food -> 2)
}

case object BronzeHoe extends TechProduct {
  override val Ingredients = Map(Wood -> 1, Bronze -> 1)
  override val LaborHours = 2
  override val Multipliers = Map(Food -> 3)
}

case object IronHoe extends TechProduct {
  override val Ingredients = Map(Wood -> 1, Iron -> 1)
  override val LaborHours = 2
  override val Multipliers = Map(Food -> 4)
}

case object CopperAxe extends TechProduct {
  override val Ingredients = Map(Wood -> 1, Copper -> 1)
  override val LaborHours = 2
  override val Multipliers = Map(Wood -> 2)
}

case object BronzeAxe extends TechProduct {
  override val Ingredients = Map(Wood -> 1, Bronze -> 1)
  override val LaborHours = 2
  override val Multipliers = Map(Wood -> 3)
}

case object IronAxe extends TechProduct {
  override val Ingredients = Map(Wood -> 1, Iron -> 1)
  override val LaborHours = 2
  override val Multipliers = Map(Wood -> 4)
}

case object CopperPick extends TechProduct {
  override val Ingredients = Map(Wood -> 1, Copper -> 1)
  override val LaborHours = 2
  override val Multipliers = Map(Wood -> 2)
}

case object BronzePick extends TechProduct {
  override val Ingredients = Map(Wood -> 1, Bronze -> 1)
  override val LaborHours = 2
  override val Multipliers = Map(Wood -> 3)
}

case object IronPick extends TechProduct {
  override val Ingredients = Map(Wood -> 1, Iron -> 1)
  override val LaborHours = 2
  override val Multipliers = Map(Wood -> 4)
}