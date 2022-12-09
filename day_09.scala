import scala.io.Source

object Day09 {
  enum Direction { case Left, Right, Up, Down }

  case class Coord(x: Int, y: Int) {
    def step(direction: Direction): Coord = direction match {
      case Direction.Left  => copy(x = x - 1)
      case Direction.Right => copy(x = x + 1)
      case Direction.Up    => copy(y = y - 1)
      case Direction.Down  => copy(y = y + 1)
    }

    def seekTowards(other: Coord): Coord = {
      def isTouching(other: Coord): Boolean =
        (x - other.x).abs <= 1 && (y - other.y).abs <= 1

      if (isTouching(other)) {
        this
      } else {
        val xNew =
          if (other.x > x) { x + 1 }
          else if (other.x < x) { x - 1 }
          else { x }

        val yNew =
          if (other.y > y) { y + 1 }
          else if (other.y < y) { y - 1 }
          else { y }

        Coord(xNew, yNew)
      }
    }
  }

  case class RopeState(knots: List[Coord], tailVisited: Set[Coord]) {
    def step(direction: Direction): RopeState = {
      val updatedHead = knots.head.step(direction)
      val updatedKnots =
        knots.tail
          .foldLeft(List(updatedHead))((updatedParents, currentKnot) =>
            currentKnot.seekTowards(updatedParents.head) :: updatedParents
          )
          .reverse

      RopeState(
        knots = updatedKnots,
        tailVisited = tailVisited + updatedKnots.last
      )
    }
  }

  case class Instruction(direction: Direction, count: Int)

  @main def main = {
    val lines = Source.fromFile("day_09.input").getLines().toList
    val instructions = lines.map(parseLine)

    println(s"Part 1: ${simulateRope(2, instructions).tailVisited.size}")
    println(s"Part 2: ${simulateRope(10, instructions).tailVisited.size}")
  }

  def simulateRope(
      length: Int,
      instructions: List[Instruction]
  ): RopeState = {
    val initialRopeState = RopeState(
      knots = List.fill(length)(Coord(0, 0)),
      tailVisited = Set(Coord(0, 0))
    )
    instructions
      .flatMap { case Instruction(dir, count) => List.fill(count)(dir) }
      .foldLeft[RopeState](initialRopeState) { (state, direction) =>
        state.step(direction)
      }
  }

  def parseLine(line: String): Instruction = {
    val Array(directionStr, countStr) = line.split(" "): @unchecked

    val direction = directionStr match {
      case "L" => Direction.Left
      case "R" => Direction.Right
      case "U" => Direction.Up
      case "D" => Direction.Down
    }

    Instruction(direction, countStr.toInt)
  }
}
