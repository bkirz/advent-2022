import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn
import scala.collection.immutable.ArraySeq

object Day12 {
  case class Point(row: Int, col: Int) {
    def +(other: Point): Point = Point(row + other.row, col + other.col)
  }

  case class Grid(locations: IndexedSeq[IndexedSeq[Char]]) {
    val numRows = locations.size
    val numCols = locations(0).size

    val start: Point = locationOf('S')
    val end: Point = locationOf('E')

    def locationOf(char: Char): Point = {
      val (row, rowNum) =
        locations.zipWithIndex.find((row, _) => row.contains(char)).get
      val colNum = row.indexOf(char)
      Point(rowNum, colNum)
    }

    val allPoints: Set[Point] =
      Range(0, numRows)
        .flatMap(r => Range(0, numCols).map(c => Point(r, c)))
        .toSet

    def inBounds(point: Point): Boolean =
      Range(0, numRows).contains(point.row) &&
        Range(0, numCols).contains(point.col)

    val NEIGHBOR_OFFSETS = List(
      Point(0, 1),
      Point(0, -1),
      Point(1, 0),
      Point(-1, 0)
    )

    def neighbors(point: Point): List[Point] =
      val height = heightAt(point).get
      NEIGHBOR_OFFSETS.map(_ + point).filter { neighbor =>
        heightAt(neighbor).exists { neighborHeight =>
          neighborHeight - height <= 1
        }
      }

    def heightAt(point: Point): Option[Char] =
      locations.lift(point.row).flatMap(row => row.lift(point.col)).map {
        case 'S'  => 'a'
        case 'E'  => 'z'
        case char => char
      }

    def shortestDistance(origin: Point, dest: Point): Int = {
      @tailrec def recursivelyFindShortestPaths(
          node: Point,
          dest: Point,
          // Shortest known distance to Point from origin.
          // A missing entry represents infinite distance.
          distances: Map[Point, Int],
          unvisited: Set[Point]
      ): Map[Point, Int] = {
        println(
          s"Visiting $node, target: $dest, unvisited: ${unvisited.size}"
        )
        if (node == dest) { return distances }
        else {
          val updatedDistances = updateDistances(node, distances)
          val nextNode =
            updatedDistances.filterKeys(unvisited.contains(_)).minBy(_._2)._1
          val updatedUnvisited = unvisited - nextNode

          recursivelyFindShortestPaths(
            nextNode,
            end,
            updatedDistances,
            updatedUnvisited
          )
        }
      }

      def updateDistances(
          node: Point,
          distances: Map[Point, Int]
      ): Map[Point, Int] = {
        val updatedDistancesToNeighbors = this
          .neighbors(node)
          .map { neighbor =>
            val distanceThroughNode = distances(node) + 1
            val knownDistance = distances.getOrElse(neighbor, Int.MaxValue)
            (neighbor, Math.min(distanceThroughNode, knownDistance))
          }
          .toMap

        distances ++ updatedDistancesToNeighbors
      }

      recursivelyFindShortestPaths(
        node = start,
        dest = end,
        distances = Map(start -> 0),
        unvisited = allPoints - start
      )(end)
    }
  }

  @main def main = {
    val grid = Grid(
      Source
        .fromFile("day_12.input")
        .getLines()
        .toIndexedSeq
        .map(_.toIndexedSeq)
    )

    println(s"Part 1: ${grid.shortestDistance(grid.start, grid.end)}")

  }
}
