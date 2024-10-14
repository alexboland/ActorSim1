trait ResourceType {
  override def toString: String = this.getClass.getSimpleName.dropRight(1)
}

sealed trait Mineable extends ResourceType

case object Food extends ResourceType
case object Wood extends ResourceType
case object Copper extends Mineable with Metal {
  override def multiplierValue = 1
}
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

trait Metal extends ResourceType {
  def multiplierValue: Int
}

case object Bronze extends TechProduct with Metal {
  override val Ingredients = Map(Copper -> 1, Tin -> 1)
  override val LaborHours = 4
  override val Multipliers = Map()
  override def multiplierValue: Int = 3
}

case object Iron extends TechProduct with Metal {
  override val Ingredients = Map(IronOre -> 1, Coal -> 1)
  override val LaborHours = 4
  override val Multipliers = Map()
  override def multiplierValue: Int = 4
}

sealed trait Tool[A <: Metal] extends TechProduct {
  val metal: A
  override val Ingredients = Map(Wood -> 1, metal -> 1)
  override val LaborHours = 2
  protected def resourceMultiplier: ResourceType
  override val Multipliers = Map(resourceMultiplier -> metal.multiplierValue)
}

case class Axe[A <: Metal](metal: A) extends Tool[A] {
  override protected def resourceMultiplier: ResourceType = Wood
}

case class Pick[A <: Metal](metal: A) extends Tool[A] {
  override protected def resourceMultiplier: ResourceType = IronOre // Assuming Stone is the resource for Pick
}

case class Hoe[A <: Metal](metal: A) extends Tool[A] {
  override protected def resourceMultiplier: ResourceType = Food
}