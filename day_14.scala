import scala.io.Source
object Day14 {

  case class Point(x: Int, y: Int) {
    def +(other: Point): Point = Point(x + other.x, y + other.y)
  }
  case class RockPath(points: List[Point]) {
    // Technically this tests for the box formed by using the points
    // as opposite corners, but that's fine since the "box" in this case
    // is guaranteed to be a line with length in only one dimension.
    def pointOccupied(point: Point): Boolean =
      points.sliding(2).exists { case List(start, end) =>
        val xs = List(start.x, end.x)
        val ys = List(start.y, end.y)
        Range.inclusive(xs.min, xs.max).contains(point.x) &&
        Range.inclusive(ys.min, ys.max).contains(point.y)
      }

    def toPointSet: Set[Point] =
      points
        .sliding(2)
        .map { case List(start, end) =>
          val xs = List(start.x, end.x)
          val ys = List(start.y, end.y)
          Range
            .inclusive(xs.min, xs.max)
            .flatMap { x =>
              Range.inclusive(ys.min, ys.max).map { y =>
                Point(x, y)
              }
            }
            .toSet
        }
        .reduce(_.union(_))
  }

  case class Simulation(
      sandSource: Point,
      rockPaths: List[RockPath],
      simulateFloor: Boolean,
      sandAtRest: Set[Point] = Set.empty,
      complete: Boolean = false,
      // An optimization to reduce the number of points simulated,
      // since most sand units will follow the same path until they
      // reach the previously placed point. For ease of use, it is
      // modeled with the most recent point at the head.
      previousSandPath: Option[List[Point]] = None
  ) {
    val rockLocations = rockPaths.flatMap(_.toPointSet)
    val points = rockPaths.map(_.toPointSet).reduce(_.union(_))
    val minY = points.map(_.y).min
    val maxY = points.map(_.y).max
    val minX = points.map(_.x).min
    val maxX = points.map(_.x).max
    val floorHeight = maxY + 2

    val MOVEMENT_OFFSETS: Seq[Point] =
      Seq(Point(0, 1), Point(-1, 1), Point(1, 1))

    sealed trait SimulationState(path: List[Point])
    case class Active(at: Point, path: List[Point])
        extends SimulationState(path)
    case class Settled(at: Point, path: List[Point])
        extends SimulationState(path)
    case class Fallen(path: List[Point]) extends SimulationState(path)

    def simulateSand: Option[(Point, List[Point])] = {
      def stepSandSimulation(
          activeUnit: Point,
          pathSoFar: List[Point]
      ): SimulationState = {
        if (!simulateFloor && activeUnit.y + 1 > maxY) {
          return Fallen(pathSoFar)
        }

        val potentialNextPoints = MOVEMENT_OFFSETS.map(_ + activeUnit)
        potentialNextPoints.find(!pointOccupied(_)) match {
          case Some(destination) => Active(destination, activeUnit :: pathSoFar)
          case None              => Settled(activeUnit, pathSoFar)
        }
      }

      val initialState = previousSandPath match {
        case Some(h :: t) => Active(h, t)
        case None => Active(sandSource, List.empty) // No sand simulated yet!
        case Some(List()) =>
          throw new RuntimeException("This should be unreachable")
      }
      val stateSequence =
        LazyList.unfold[SimulationState, SimulationState](initialState) {
          case Active(point, path) =>
            val stepped = stepSandSimulation(point, path)
            Some((stepped, stepped))
          case other => None
        }
      stateSequence.last match {
        case Settled(point, path) => Some(point, path)
        case Fallen(_)            => None
        case Active(_, _) =>
          throw new RuntimeException(
            "Unexpected active sand unit at the end of a simulation"
          )
      }
    }

    def step: Simulation = {
      if (pointOccupied(sandSource)) { copy(complete = true) }
      else {
        simulateSand match {
          case Some((point, path)) =>
            copy(
              sandAtRest = sandAtRest + point,
              previousSandPath = Some(path)
            )
          case None => copy(complete = true)
        }
      }
    }

    def pointOccupied(point: Point): Boolean =
      sandAtRest.contains(point) ||
        rockLocations.contains(point) ||
        (simulateFloor && point.y == floorHeight)

    override def toString: String = {

      val lines = Range
        .inclusive(0, maxY + 2)
        .map { y =>
          Range
            .inclusive(minX, maxX)
            .map { x =>
              val point = Point(x, y)
              if (sandAtRest.contains(point)) {
                'o'
              } else if (
                rockLocations.contains(point) ||
                (simulateFloor && point.y == maxY + 2)
              ) {
                '#'
              } else { '.' }
            }
            .mkString("")
        }

      lines.mkString("\n")
    }
  }

  def main = {
    val lines = Source.fromFile("day_14.input").getLines().toList
    val rockPaths = lines.map(parseLine)
    val sandSource = Point(500, 0)
    val part1Simulation = Simulation(
      sandSource = sandSource,
      rockPaths = rockPaths,
      simulateFloor = false
    )

    val part1TerminalState =
      LazyList.iterate(part1Simulation)(_.step).takeWhile(!_.complete).last

    println(s"Part 1: ${part1TerminalState.sandAtRest.size}")

    val part2Simulation = Simulation(
      sandSource = sandSource,
      rockPaths = rockPaths,
      simulateFloor = true
    )

    val part2TerminalState =
      LazyList.iterate(part2Simulation)(_.step).takeWhile(!_.complete).last
    println(s"Part 2: ${part2TerminalState.sandAtRest.size}")

  }

  def parseLine(line: String): RockPath =
    RockPath(
      line
        .split(" -> ")
        .map { pointStr =>
          val Array(x, y) = pointStr.split(",")
          Point(x.toInt, y.toInt)
        }
        .toList
    )
}
