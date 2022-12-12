import scala.io.Source

enum Direction { case North, East, South, West }

case class Coord(row: Int, col: Int) {
  def step(direction: Direction): Coord = direction match {
    case Direction.North => copy(row = row - 1)
    case Direction.South => copy(row = row + 1)
    case Direction.East  => copy(col = col - 1)
    case Direction.West  => copy(col = col + 1)
  }
}

class Forest(grid: Array[Array[Int]]) {
  val rowCount = grid.length
  val colCount = grid(0).length

  def allCoords: Set[Coord] =
    0.until(rowCount)
      .flatMap { row =>
        0.until(colCount).map { col =>
          Coord(row, col)
        }
      }
      .toSet

  def heightOf(coord: Coord): Int = grid(coord.row)(coord.col)

  def inBounds(coord: Coord): Boolean =
    Range(0, rowCount).contains(coord.row) &&
      Range(0, colCount).contains(coord.col)

  def isVisible(coord: Coord): Boolean = {
    val height = heightOf(coord)

    def visibleFrom(direction: Direction): Boolean = {
      Stream
        .iterate(coord)(_.step(direction))
        .drop(1) // Don't consider the current coord
        .takeWhile(inBounds(_))
        .forall(heightOf(_) < height)
    }

    Direction.values.exists(visibleFrom)
  }

  def scenicScore(coord: Coord): Int = {
    val height = heightOf(coord)

    def uninterruptedViewLength(direction: Direction): Int =
      Stream
        .iterate(coord)(_.step(direction))
        /* This is needed to get around the annoying requirement
         * that we do count the first tree >= the current square
         * but we don't count the coordinate past the edge of the forest.
         * The sliding window lets us look ahead one square for height,
         * but not for boundary checks. */
        .sliding(2)
        .map(_.toList)
        .takeWhile {
          case List(prev: Coord, current: Coord) =>
            inBounds(current) && (coord == prev || heightOf(prev) < height)
          case _ => throw new RuntimeException("This should be unreachable")
        }
        .length

    Direction.values.map(uninterruptedViewLength).product
  }
}

object Day08 {
  def main = {
    val lines = Source.fromFile("day_08.input").getLines().toList
    val forest = Forest(lines.map { (row: String) =>
      row.toCharArray.map(_.toInt)
    }.toArray)

    println(f"Part 1: ${forest.allCoords.count(forest.isVisible(_))}")
    println(f"Part 2: ${forest.allCoords.map(forest.scenicScore(_)).max}")
  }
}
