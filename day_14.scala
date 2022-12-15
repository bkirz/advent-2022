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
  }

  case class Simulation(
      sandSource: Point,
      rockPaths: List[RockPath],
      sandAtRest: Set[Point] = Set.empty
  ) {
    val points = rockPaths.flatMap(_.points)
    val minY = points.map(_.y).min
    val maxY = points.map(_.y).max
    val minX = points.map(_.x).min
    val maxX = points.map(_.x).max

    val MOVEMENT_OFFSETS: Seq[Point] =
      Seq(Point(0, 1), Point(-1, 1), Point(1, 1))

    sealed trait SimulationState
    case class Active(at: Point) extends SimulationState
    case class Settled(at: Point) extends SimulationState
    case object Fallen extends SimulationState

    def simulateSand: Option[Point] = {
      def stepSandSimulation(activeUnit: Point): SimulationState = {
        if (activeUnit.y + 1 > maxY) { return Fallen }

        val potentialNextPoints = MOVEMENT_OFFSETS.map(_ + activeUnit)
        potentialNextPoints.find(!pointOccupied(_)) match {
          case Some(destination) => Active(destination)
          case None              => Settled(activeUnit)
        }
      }

      val initialState = Active(sandSource)
      val stateSequence =
        LazyList.unfold[SimulationState, SimulationState](initialState) {
          case Active(point) =>
            val stepped = stepSandSimulation(point)
            Some((stepped, stepped))
          case other => None
        }
      stateSequence.last match {
        case Settled(point) => Some(point)
        case Fallen         => None
        case Active(_) =>
          throw new RuntimeException(
            "Unexpected active sand unit at the end of a simulation"
          )
      }
    }

    def step: Simulation = {
      simulateSand match {
        case Some(point) => copy(sandAtRest = sandAtRest + point)
        case None        => this
      }
    }

    def isComplete: Boolean = simulateSand.isEmpty

    def pointOccupied(point: Point): Boolean =
      sandAtRest.contains(point) || rockPaths.exists(_.pointOccupied(point))

    override def toString: String = {
      val lines = Range
        .inclusive(0, maxY)
        .map { y =>
          Range
            .inclusive(minX, maxX)
            .map { x =>
              val point = Point(x, y)
              if (sandAtRest.contains(point)) {
                'o'
              } else if (rockPaths.exists(_.pointOccupied(point))) {
                '#'
              } else { '.' }
            }
            .mkString("")
        }

      lines.mkString("\n")
    }
  }

  @main def main = {
    val lines = Source.fromFile("day_14.input").getLines().toList
    val rockPaths = lines.map(parseLine)
    val simulation = Simulation(
      sandSource = Point(500, 0),
      rockPaths = rockPaths
    )

    val iterations =
      LazyList.iterate(simulation)(_.step).takeWhile(!_.isComplete)
    // takeWhile stops one step short of the final iteration.
    val terminalState = iterations.last.step

    println(terminalState)
    println(s"Part 1: ${terminalState.sandAtRest.size}")
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
