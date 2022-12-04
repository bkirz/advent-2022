import scala.io.Source

object Day03 {
  @main def main() = {
    val lines = Source.fromFile("day_03.input").getLines().toList

    val dupedItemsPerRucksackPriority =
      lines
        .map(duplicatedItemInRucksack)
        .map(itemPriority)
        .sum

    val dupedItemsAcrossTeamsPriority =
      lines
        .grouped(3)
        .map(duplicatedItemAcrossRucksacks)
        .map(itemPriority)
        .sum

    println(f"Part 1: ${dupedItemsPerRucksackPriority}")
    println(f"Part 2: ${dupedItemsAcrossTeamsPriority}")
  }

  def duplicatedItemInRucksack(line: String): Char = {
    val (firstCompartment: Set[Char], secondCompartment: Set[Char]) =
      line.splitAt(line.length / 2).map(_.toSet)
    val commonItems: Set[Char] = firstCompartment.intersect(secondCompartment)
    val List(item: Char) = commonItems.toList
    item
  }

  def duplicatedItemAcrossRucksacks(lines: List[String]): Char = {
    val commonItems: Set[Char] = lines.map(_.toSet).reduce(_.intersect(_))
    val List(item: Char) = commonItems.toList
    item
  }

  def itemPriority(item: Char): Int = {
    if ('a' <= item && item <= 'z') {
      item.toInt - 'a' + 1
    } else if ('A' <= item && item <= 'Z') {
      item.toInt - 'A' + 27
    } else {
      throw new IllegalArgumentException(f"Unexpected character: ${item}")
    }
  }
}
