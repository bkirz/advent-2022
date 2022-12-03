import scala.io.Source

object Day03 {
  @main def main() = {
    val lines = Source.fromFile("day_03.input").getLines().toList
    println(
      f"Part 1: Total priorities of duplicated items: ${lines.map(priorityOfDuplicatedItem).sum}"
    )
  }

  def priorityOfDuplicatedItem(line: String): Int = {
    val (firstPart, secondPart) = line.splitAt(line.length / 2)
    val firstCompartmentItems: Set[Char] = firstPart.toSet
    val secondCompartmentItems: Set[Char] = secondPart.toSet

    val commonItems: Set[Char] =
      firstCompartmentItems.intersect(secondCompartmentItems)
    commonItems.toList match {
      case List(item: Char) => charPriority(item)
      case _ =>
        throw new IllegalArgumentException(
          f"Unexpectedly found multiple duplicated items in rucksack: ${line}, dupes: ${commonItems}"
        )
    }
  }

  def charPriority(char: Char): Int = {
    if ('a' <= char && char <= 'z') {
      char.toInt - 'a' + 1
    } else if ('A' <= char && char <= 'Z') {
      char.toInt - 'A' + 27
    } else {
      throw new IllegalArgumentException(f"Unexpected character: ${char}")
    }
  }
}
