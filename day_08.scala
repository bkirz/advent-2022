import scala.io.Source

case class Coord(row: Int, col: Int)

enum Direction { case North, East, South, West }

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
      val step: (Coord) => Coord = direction match {
        case Direction.North => (coord) => coord.copy(row = coord.row - 1)
        case Direction.South => (coord) => coord.copy(row = coord.row + 1)
        case Direction.East  => (coord) => coord.copy(col = coord.col - 1)
        case Direction.West  => (coord) => coord.copy(col = coord.col + 1)
      }

      var currentCoord = step(coord)
      while (inBounds(currentCoord)) {
        if (heightOf(currentCoord) >= height) { return false }
        currentCoord = step(currentCoord)
      }
      return true
    }

    Direction.values.exists(visibleFrom)
  }
}

object Day08 {
  @main def main = {
    val lines = Source.fromFile("day_08.input").getLines().toList
    val forest = Forest(lines.map { (row: String) =>
      row.toCharArray.map(_.toInt)
    }.toArray)

    println(f"Part 1: ${forest.allCoords.count(forest.isVisible(_))}")
  }
}
