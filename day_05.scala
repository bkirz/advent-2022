import scala.io.Source
import scala.util.matching.Regex

case class CargoState(stacks: Array[List[Char]]) {
  def moveCargo(instruction: MoveInstruction, invert: Boolean): CargoState = {
    val fromStack = stacks(instruction.from)
    val toStack = stacks(instruction.to)

    val (cargoToMove: List[Char], updatedFromStack: List[Char]) =
      fromStack.splitAt(instruction.count)
    val updatedToStack: List[Char] =
      toStack.prependedAll(if invert then cargoToMove.reverse else cargoToMove)

    CargoState(
      stacks
        .updated(instruction.from, updatedFromStack)
        .updated(instruction.to, updatedToStack)
    )
  }

  def topCrateString: String = stacks.map(_.head).mkString
}
case class MoveInstruction(count: Int, from: Int, to: Int)

object Day05 {
  // This is a huge hack, but the structure of the initial cargo state in the
  // input is _very_ annoying to parse. When I have time, I might go back and
  // attempt the following state building algo:
  //   - reverse the top half of lines such that the first is an index by
  //     column. build a Map[index, List[Char].empty] and a Map[index, column offset].
  //   - for each remaining line (now from the base of the list), push values onto each
  //     stack by iterating through the index->column offset map and finding the char
  //     at each offset.
  val INITIAL_CARGO_STATE = CargoState(
    Array(
      List('G', 'P', 'N', 'R'),
      List('H', 'V', 'S', 'C', 'L', 'B', 'J', 'T'),
      List('L', 'N', 'M', 'B', 'D', 'T'),
      List('B', 'S', 'P', 'V', 'R'),
      List('H', 'V', 'M', 'W', 'S', 'Q', 'C', 'G'),
      List('J', 'B', 'D', 'C', 'S', 'Q', 'W'),
      List('L', 'Q', 'F'),
      List('V', 'F', 'L', 'D', 'T', 'H', 'M', 'W'),
      List('F', 'J', 'M', 'V', 'B', 'P', 'L')
    )
  )
  @main def main = {
    val lines = Source.fromFile("day_05.input").getLines().toList
    val splitIndex = lines.indexOf("")
    val (_cargoLines, moveLines) = lines.splitAt(splitIndex)

    val moveInstructions: List[MoveInstruction] =
      moveLines
        .drop(1) // Drop the blank line
        .map(parseMoveInstruction)

    val finalCargoStateWithInverts =
      moveInstructions.foldLeft(INITIAL_CARGO_STATE)((cargoState, instr) =>
        cargoState.moveCargo(instr, invert = true)
      )

    println(f"Part 1: ${finalCargoStateWithInverts.topCrateString}")

    val finalCargoStateWithoutInverts =
      moveInstructions.foldLeft(INITIAL_CARGO_STATE)((cargoState, instr) =>
        cargoState.moveCargo(instr, invert = false)
      )

    println(f"Part 2: ${finalCargoStateWithoutInverts.topCrateString}")
  }

  val MOVEMENT_PATTERN = """move (\d+) from (\d+) to (\d+)""".r
  def parseMoveInstruction(line: String): MoveInstruction = {
    line match {
      case MOVEMENT_PATTERN(count, from, to) =>
        MoveInstruction(count.toInt, from.toInt - 1, to.toInt - 1)
      case _ =>
        throw new IllegalArgumentException(f"Invalid movement line: $line")
    }
  }
}
