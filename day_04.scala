import scala.io.Source
import scala.util.matching.Regex

case class RangePair(first: Range, second: Range) {
  def fullOverlap: Boolean = {
    (first.contains(second.start) && first.contains(second.end)) ||
    (second.contains(first.start) && second.contains(first.end))
  }

  def partialOverlap: Boolean = {
    first.contains(second.start) || first.contains(second.end) ||
    second.contains(first.start) || second.contains(first.end)
  }
}

object Day04 {
  val LINE_PATTERN = "(\\d+)-(\\d+),(\\d+)-(\\d+)".r
  def parseLine(line: String): RangePair = {
    line match {
      case LINE_PATTERN(firstStart, firstEnd, secondStart, secondEnd) =>
        RangePair(
          Range.inclusive(firstStart.toInt, firstEnd.toInt),
          Range.inclusive(secondStart.toInt, secondEnd.toInt)
        )
      case _ => throw new IllegalArgumentException(f"Unparseable line ${line}")
    }
  }

  @main def main = {
    val lines = Source.fromFile("day_04.input").getLines().toList
    println(f"Part 1: ${lines.map(parseLine).filter(_.fullOverlap).length}")
    println(f"Part 2: ${lines.map(parseLine).filter(_.partialOverlap).length}")
  }
}
