package middleware

import akka.actor.typed.ActorRef
import java.time.Instant
import scala.collection.immutable.{SortedMap, TreeMap}

// EventType trait and common event types
sealed trait EventType {
  def name: String
}

object EventType {
  // Common event types
  case object ResourceProduced extends EventType { val name = "ResourceProduced" }
  case object ResourceConsumed extends EventType { val name = "ResourceConsumed" }
  case object PopulationChanged extends EventType { val name = "PopulationChanged" }
  case object MarketTransaction extends EventType { val name = "MarketTransaction" }
  case object BondIssued extends EventType { val name = "BondIssued" }
  case object BondRepaid extends EventType { val name = "BondRepaid" }
  case object WorkerHired extends EventType { val name = "WorkerHired" }
  case object FounderCreated extends EventType { val name = "FounderCreated" }
  case object ConstructionStarted extends EventType { val name = "ConstructionStarted" }
  case object ConstructionCompleted extends EventType { val name = "ConstructionCompleted" }
  case object SeasonChanged extends EventType { val name = "SeasonChanged" }

  // Add custom event types as needed
  case class Custom(override val name: String) extends EventType
}

// GameEvent case class
case class GameEvent(
                      timestamp: Long,       // Unix timestamp (milliseconds since epoch)
                      agentId: String,       // ID of the agent involved in the event
                      regionId: String,      // ID of the region where the event occurred
                      eventType: EventType,  // Type of event
                      eventText: String      // Human-readable description of the event
                    ) {
  // Helper method to get a formatted timestamp
  def formattedTime: String = Instant.ofEpochMilli(timestamp).toString

  override def toString: String =
    s"[$formattedTime] $regionId - $agentId: ${eventType.name} - $eventText"
}

// GameHistory with efficient indexing and search capabilities
// Custom TimeRange class for event partitions
case class TimeRange(start: Int, end: Int)

case class GameHistory(
                        events: Vector[GameEvent] = Vector.empty,
                        // Indexes for efficient searching
                        agentIndex: Map[String, Set[Int]] = Map.empty,      // agentId -> indices in events
                        regionIndex: Map[String, Set[Int]] = Map.empty,     // regionId -> indices in events
                        typeIndex: Map[EventType, Set[Int]] = Map.empty,    // eventType -> indices in events
                        // Time-based partitioning for efficient timestamp range queries
                        timePartitions: SortedMap[Long, TimeRange] = TreeMap.empty  // partition start time -> range in events
                      ) {

  // Partition size in milliseconds (e.g., 1 hour)
  private val PartitionSize: Long = 3600000L

  /**
   * Add a new event to the history
   */
  def addEvent(event: GameEvent): GameHistory = {
    // Find insertion point to maintain timestamp order
    val insertionIndex = binarySearchInsertionPoint(events, event, _.timestamp)

    // Insert event
    val newEvents = events.patch(insertionIndex, Vector(event), 0)

    // Update indexes with the new event
    val newAgentIndex = updateIndex(agentIndex, event.agentId, insertionIndex)
    val newRegionIndex = updateIndex(regionIndex, event.regionId, insertionIndex)
    val newTypeIndex = updateIndex(typeIndex, event.eventType, insertionIndex)

    // Update time partitions
    val partitionKey = event.timestamp - (event.timestamp % PartitionSize)
    val newPartitions = updateTimePartitions(timePartitions, partitionKey, insertionIndex)

    // Return updated history
    GameHistory(
      newEvents,
      newAgentIndex,
      newRegionIndex,
      newTypeIndex,
      newPartitions
    )
  }

  /**
   * Helper method to find the correct insertion point for maintaining timestamp order
   */
  private def binarySearchInsertionPoint[T, V](seq: Vector[T], target: T, getValue: T => V)
                                              (implicit ord: Ordering[V]): Int = {
    var low = 0
    var high = seq.size - 1

    while (low <= high) {
      val mid = (low + high) >>> 1
      val midVal = getValue(seq(mid))
      val targetVal = getValue(target)

      val cmp = ord.compare(midVal, targetVal)
      if (cmp < 0)
        low = mid + 1
      else if (cmp > 0)
        high = mid - 1
      else
        return mid
    }

    low
  }

  /**
   * Update an index with a new event index
   */
  private def updateIndex[K](index: Map[K, Set[Int]], key: K, newIndex: Int): Map[K, Set[Int]] = {
    // Shift all indices that are >= newIndex
    val updatedIndex = index.map { case (k, indices) =>
      k -> indices.map(i => if (i >= newIndex) i + 1 else i)
    }

    // Add the new index
    updatedIndex.updated(key, updatedIndex.getOrElse(key, Set.empty) + newIndex)
  }

  /**
   * Update time partitions for efficient time-range queries
   */
  private def updateTimePartitions(
                                    partitions: SortedMap[Long, TimeRange],
                                    partitionKey: Long,
                                    insertionIndex: Int
                                  ): SortedMap[Long, TimeRange] = {
    // Adjust all ranges after the insertion point
    val adjustedPartitions = partitions.map { case (time, range) =>
      if (range.start >= insertionIndex) {
        time -> TimeRange(range.start + 1, range.end + 1)
      } else if (range.end > insertionIndex) {
        time -> TimeRange(range.start, range.end + 1)
      } else {
        time -> TimeRange(range.start, range.end)
      }
    }

    // Add or update the partition for this event
    adjustedPartitions.get(partitionKey) match {
      case Some(range) =>
        if (insertionIndex >= range.start && insertionIndex <= range.end) {
          // Event is within the existing range
          adjustedPartitions
        } else {
          // Create a new range that includes this index
          adjustedPartitions.updated(partitionKey, TimeRange(Math.min(range.start, insertionIndex), range.end))
        }
      case None =>
        // Create new partition with a single event
        adjustedPartitions + (partitionKey -> TimeRange(insertionIndex, insertionIndex + 1))
    }
  }

  /**
   * Define ordering options for search results
   */
  object OrderBy {
    sealed trait Field
    case object Timestamp extends Field
    case object AgentId extends Field
    case object RegionId extends Field
    case object EventType extends Field

    sealed trait Direction
    case object Ascending extends Direction
    case object Descending extends Direction
  }

  /**
   * Search for events with various criteria
   * @param orderBy.field The field to order results by (default: Timestamp)
   * @param orderBy.direction The direction to order results (default: Descending for newest first)
   */
  def search(
              agentId: Option[String] = None,
              regionId: Option[String] = None,
              eventType: Option[EventType] = None,
              fromTime: Option[Long] = None,
              toTime: Option[Long] = None,
              offset: Int = 0,
              limit: Int = 100,
              orderBy: (OrderBy.Field, OrderBy.Direction) = (OrderBy.Timestamp, OrderBy.Descending)
            ): (Vector[GameEvent], Int) = {
    // First, narrow down by time range if specified
    val timeRanges = if (fromTime.isDefined || toTime.isDefined) {
      val fromKey = fromTime.map(t => t - (t % PartitionSize)).getOrElse(0L)
      val toKey = toTime.map(t => t - (t % PartitionSize) + PartitionSize).getOrElse(Long.MaxValue)

      // Get all partitions within the time range
      timePartitions.range(fromKey, toKey).values.toVector
    } else {
      // If no time filter, consider all events
      Vector(TimeRange(0, events.size))
    }

    // Get candidate indices from time partitions
    val timeIndices = timeRanges.flatMap(range => range.start until range.end).toSet

    // Intersect with other criteria
    val filteredIndices = intersectIndices(
      timeIndices,
      agentId.map(id => agentIndex.getOrElse(id, Set.empty)).getOrElse(timeIndices),
      regionId.map(id => regionIndex.getOrElse(id, Set.empty)).getOrElse(timeIndices),
      eventType.map(et => typeIndex.getOrElse(et, Set.empty)).getOrElse(timeIndices)
    )

    // Apply precise time filtering
    val finalIndices = filteredIndices
      .filter(i => {
        val event = events(i)
        (fromTime.isEmpty || event.timestamp >= fromTime.get) &&
          (toTime.isEmpty || event.timestamp <= toTime.get)
      })
      .toVector
      .sorted

    // Get events for the matching indices
    val matchingEvents = finalIndices.map(events(_))

    // Apply ordering
    val orderedEvents = orderBy match {
      case (field, direction) =>
        val ordered = field match {
          case OrderBy.Timestamp => matchingEvents.sortBy(_.timestamp)
          case OrderBy.AgentId => matchingEvents.sortBy(_.agentId)
          case OrderBy.RegionId => matchingEvents.sortBy(_.regionId)
          case OrderBy.EventType => matchingEvents.sortBy(_.eventType.name)
        }

        direction match {
          case OrderBy.Ascending => ordered
          case OrderBy.Descending => ordered.reverse
        }
    }

    // Apply pagination
    val paginatedEvents = orderedEvents.slice(offset, offset + limit)

    (paginatedEvents, matchingEvents.size) // Return events and total count
  }

  /**
   * Helper method to intersect multiple sets of indices
   */
  private def intersectIndices(sets: Set[Int]*): Set[Int] = {
    if (sets.isEmpty) Set.empty
    else sets.reduce(_ intersect _)
  }

  /**
   * Find events within a time range
   */
  def findByTimeRange(from: Long, to: Long, ascending: Boolean = false): Vector[GameEvent] = {
    val direction = if (ascending) OrderBy.Ascending else OrderBy.Descending
    val (results, _) = search(
      fromTime = Some(from),
      toTime = Some(to),
      orderBy = (OrderBy.Timestamp, direction)
    )
    results
  }

  /**
   * Find events by agent ID
   */
  def findByAgent(agentId: String, ascending: Boolean = false): Vector[GameEvent] = {
    val direction = if (ascending) OrderBy.Ascending else OrderBy.Descending
    val (results, _) = search(
      agentId = Some(agentId),
      orderBy = (OrderBy.Timestamp, direction)
    )
    results
  }

  /**
   * Find events by region ID
   */
  def findByRegion(regionId: String, ascending: Boolean = false): Vector[GameEvent] = {
    val direction = if (ascending) OrderBy.Ascending else OrderBy.Descending
    val (results, _) = search(
      regionId = Some(regionId),
      orderBy = (OrderBy.Timestamp, direction)
    )
    results
  }

  /**
   * Find events by event type
   */
  def findByEventType(eventType: EventType, ascending: Boolean = false): Vector[GameEvent] = {
    val direction = if (ascending) OrderBy.Ascending else OrderBy.Descending
    val (results, _) = search(
      eventType = Some(eventType),
      orderBy = (OrderBy.Timestamp, direction)
    )
    results
  }

  /**
   * Get the total number of events in the history
   */
  def size: Int = events.size

  /**
   * Get the most recent events
   */
  def recent(n: Int = 10): Vector[GameEvent] = {
    val (results, _) = search(
      limit = n,
      orderBy = (OrderBy.Timestamp, OrderBy.Descending)
    )
    results
  }
}