import scala.io.Source

object Day03 {
  @main def main() = {
    val lines = Source.fromFile("day_03.input").getLines().toList
    println(
      f"Part 1: Total priorities of duplicated items: ${lines.map(duplicatedItemInRucksack).map(itemPriority).sum}"
    )
    println(
      f"Part 2: Total priorities of common items across: ${lines.grouped(3).map(duplicatedItemAcrossRucksacks).map(itemPriority).sum}"
    )
  }

  def duplicatedItemInRucksack(line: String): Char = {
    val (firstPart, secondPart) = line.splitAt(line.length / 2)
    val firstCompartmentItems: Set[Char] = firstPart.toSet
    val secondCompartmentItems: Set[Char] = secondPart.toSet

    val commonItems: Set[Char] =
      firstCompartmentItems.intersect(secondCompartmentItems)
    commonItems.toList match {
      case List(item: Char) => item
      case _ =>
        throw new IllegalArgumentException(
          f"Unexpectedly found multiple duplicated items in rucksack: ${line}, dupes: ${commonItems}"
        )
    }
  }

  def duplicatedItemAcrossRucksacks(lines: List[String]): Char = {
    val commonItems: Set[Char] = lines.map(_.toSet).reduce(_.intersect(_))
    commonItems.toList match {
      case List(item: Char) => item
      case _ =>
        throw new IllegalArgumentException(
          f"Unexpectedly found multiple duplicated items in rucksack set: ${lines}, dupes: ${commonItems}"
        )
    }
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
