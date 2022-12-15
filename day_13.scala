import scala.io.Source
object Day13 {
  sealed trait Packet extends Ordered[Packet]
  case class IntPacket(value: Int) extends Packet {
    def compare(that: Packet): Int = that match {
      case IntPacket(thatVal) => value - thatVal
      case ListPacket(_)      => ListPacket(List(this)).compare(that)
    }
  }
  case class ListPacket(values: List[Packet]) extends Packet {
    def compare(that: Packet): Int = (values, that) match {
      case (_, IntPacket(thatVal))      => compare(ListPacket(List(that)))
      case (List(), ListPacket(List())) => 0
      case (_ :: _, ListPacket(List())) => 1
      case (List(), ListPacket(_ :: _)) => -1
      case (thisHead :: thisTail, ListPacket(thatHead :: thatTail)) =>
        val headComp = thisHead.compare(thatHead)
        if (headComp == 0) {
          ListPacket(thisTail).compare(ListPacket(thatTail))
        } else {
          headComp
        }
    }
  }

  case class PacketPair(left: Packet, right: Packet) {
    def isInRightOrder: Boolean = left <= right

    def toList: List[Packet] = List(left, right)
  }

  def main = {
    val lines = Source.fromFile("day_13.input").getLines().toList
    val pairs = parseLines(lines)
    val validIndices =
      pairs.zipWithIndex
        .filter { case (pair, _index) => pair.isInRightOrder }
        .map { case (_pair, index) => index + 1 }

    println(s"Part 1: ${validIndices.sum}")

    val dividerPackets =
      List(
        ListPacket(List(IntPacket(2))),
        ListPacket(List(IntPacket(6)))
      )
    val sortedPacketsWithDividers = pairs
      .flatMap(_.toList)
      .appendedAll(dividerPackets)
      .sorted

    val decoderKey = dividerPackets.map { divider =>
      sortedPacketsWithDividers.indexOf(divider) + 1
    }.product

    println(s"Part 2: ${decoderKey}")
  }

  def parseLines(lines: List[String]): List[PacketPair] =
    lines
      .filter(!_.isEmpty)
      .grouped(2)
      .map {
        case List(left, right) =>
          PacketPair(parseLine(left), parseLine(right))
        case other =>
          throw new IllegalArgumentException(
            s"Unable to parse packet pair: $other"
          )
      }
      .toList

  val INT_PATTERN = """\d+""".r
  // Parse using a stack:
  // - "["
  def parseLine(line: String): Packet = {
    def parseSinglePacket(
        line: String
    ): (Packet, String) = {
      INT_PATTERN.findPrefixMatchOf(line).foreach { m =>
        return (IntPacket(m.matched.toInt), m.after.toString)
      }

      if (line.startsWith("[]")) {
        return (ListPacket(List.empty), line.slice(2, line.length))
      }

      if (line.startsWith("[")) {
        val initialState: (List[Packet], String) =
          (List.empty, line.slice(1, line.length))
        val (elements, rest) = LazyList
          .iterate(initialState)(parseListElem)
          .dropWhile { case (_, rest) =>
            rest.charAt(0) != ']'
          }
          .head

        return (ListPacket(elements.reverse), rest.slice(1, rest.length))
      }

      throw new IllegalArgumentException("unreachable")
    }
    def parseListElem(
        parseState: (List[Packet], String)
    ): (List[Packet], String) = parseState match {
      case (packetsSoFar, line): (List[Packet], String) =>
        if (line.charAt(0) == ',') {
          parseListElem((packetsSoFar, line.slice(1, line.length)))
        } else {
          val (packet, rest): (Packet, String) = parseSinglePacket(line)
          (packet :: packetsSoFar, rest)
        }
    }

    val (packet, rest) = parseSinglePacket(line)
    require(rest.isEmpty, s"Unexpected trailing data at end of line: $rest")
    packet
  }
}
