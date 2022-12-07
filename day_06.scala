import scala.io.Source

object Day06 {
  def main = {
    val List(input) = Source.fromFile("day_06.input").getLines().toList

    println(f"Part 1: ${findMarker(input, 4)}")
    println(f"Part 2: ${findMarker(input, 14)}")
  }

  def findMarker(message: String, packetLength: Int) =
    message.iterator.sliding(packetLength).indexWhere { (chars: Seq[Char]) =>
      chars.toSet.size == packetLength
    } + packetLength
}
