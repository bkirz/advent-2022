import scala.io.Source
import scala.collection.immutable.SortedSet

extension (r: Range.Inclusive) {
  def clamped(by: Range.Inclusive): Range.Inclusive =
    Range.inclusive(Math.max(r.start, by.start), Math.min(r.end, by.end))
}

object Day15 {
  case class Coord(x: Int, y: Int) {
    def manhattanDistance(that: Coord) =
      Math.abs(this.x - that.x) + Math.abs(this.y - that.y)

    val tuningFrequency: BigInt = BigInt(x) * 4_000_000 + BigInt(y)
  }

  case class SensorReading(sensor: Coord, closestBeacon: Coord) {
    val radius = sensor.manhattanDistance(closestBeacon)

    def rangeCoveredInRow(row: Int): Range.Inclusive = {
      val xDistance = radius - Math.abs(row - sensor.y)
      Range.inclusive(sensor.x - xDistance, sensor.x + xDistance)
    }
  }

  def consolidateRanges(
      ranges: List[Range.Inclusive]
  ): List[Range.Inclusive] = {
    ranges
      .sortBy(_.start)
      .foldLeft(List.empty[Range.Inclusive]) { (acc, r) =>
        acc match {
          case head :: tail if head.contains(r.start) && head.contains(r.end) =>
            // r completely contained; drop it
            head :: tail
          case head :: tail if head.contains(r.start) =>
            // partial overlap; expand head to include both head and r
            Range.inclusive(head.start, r.end) :: tail
          case _ =>
            // no overlap; prepend r to list
            r :: acc
        }
      }
  }

  def findCoverageGap(
      toCover: Range.Inclusive,
      ranges: List[Range.Inclusive]
  ): Option[Int] = {
    val consolidatedRanges = consolidateRanges(
      ranges.map(_.clamped(toCover)).filterNot(_.isEmpty)
    ).reverse

    consolidatedRanges match {
      case List(range) =>
        if (range == toCover) {
          None
        } else if (range == Range.inclusive(toCover.start + 1, toCover.end)) {
          Some(0)
        } else if (range == Range.inclusive(toCover.start, toCover.end - 1)) {
          Some(toCover.end)
        } else {
          throw new RuntimeException(
            s"Invalid coverage of $toCover: $consolidatedRanges"
          )
        }
      case List(range1, range2) =>
        require(
          range2.start - range1.end == 2,
          s"Invalid coverage of $toCover: $consolidatedRanges"
        )
        Some(range1.end + 1)
      case _ =>
        throw new RuntimeException(
          s"Invalid coverage of $toCover: $consolidatedRanges"
        )
    }
  }

  val DISTRESS_SIGNAL_BOUNDS = Range.inclusive(0, 4_000_000)

  def main = {
    val lines = Source.fromFile("day_15.input").getLines().toList
    val sensorReadings = lines.map(parseLine)

    val part1Row = 2_000_000
    val ranges: List[Range.Inclusive] =
      sensorReadings.map(_.rangeCoveredInRow(part1Row)).filterNot(_.isEmpty)
    val consolidatedRanges: List[Range] = consolidateRanges(ranges)

    println(s"Part 1: ${consolidatedRanges.map(_.size).sum}")

    val gapCoordinates =
      DISTRESS_SIGNAL_BOUNDS
        .map { y =>
          val consolidatedRanges = consolidateRanges(
            sensorReadings.map(_.rangeCoveredInRow(y)).filterNot(_.isEmpty)
          )
          (y, findCoverageGap(DISTRESS_SIGNAL_BOUNDS, consolidatedRanges))
        }
        .collectFirst { case (y, Some(x)) => Coord(x, y) }
        .get

    println(s"Part 2: ${gapCoordinates.tuningFrequency}")
  }

  val PATTERN =
    """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""".r

  def parseLine(line: String): SensorReading = {
    line match {
      case PATTERN(sensorX, sensorY, beaconX, beaconY) =>
        SensorReading(
          sensor = Coord(sensorX.toInt, sensorY.toInt),
          closestBeacon = Coord(beaconX.toInt, beaconY.toInt)
        )
      case _ => throw new IllegalArgumentException(s"Invalid input $line")
    }
  }
}
