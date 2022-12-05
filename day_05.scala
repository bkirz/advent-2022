import scala.io.Source
import scala.util.matching.Regex

case class MoveInstruction(count: Int, from: Int, to: Int)

object CargoState {
  def empty(size: Int): CargoState =
    CargoState(Seq.fill(size) { List[Char]() })
}

case class CargoState(stacks: Seq[List[Char]]) {
  def appendRow(cargoRow: Seq[Option[Char]]): CargoState =
    CargoState(stacks.zip(cargoRow).map { (cargoStack, maybeCrate) =>
      maybeCrate.map(_ :: cargoStack).getOrElse(cargoStack)
    })

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

object Day05 {
  @main def main = {
    val lines = Source.fromFile("day_05.input").getLines().toSeq
    val splitIndex = lines.indexOf("")
    val (cargoLines, moveLines) = lines.splitAt(splitIndex)

    val moveInstructions: Seq[MoveInstruction] =
      moveLines
        .drop(1) // Drop the blank line
        .map(parseMoveInstruction)

    val initialCargoState = parseInitialCargoState(cargoLines)

    val finalCargoStateWithInverts =
      moveInstructions.foldLeft(initialCargoState)((cargoState, instr) =>
        cargoState.moveCargo(instr, invert = true)
      )

    println(f"Part 1: ${finalCargoStateWithInverts.topCrateString}")

    val finalCargoStateWithoutInverts =
      moveInstructions.foldLeft(initialCargoState)((cargoState, instr) =>
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

  val INDEX_PATTERN = """\d+""".r
  def parseInitialCargoState(lines: Seq[String]): CargoState = {
    val (cargoLines, Seq(indexesLine)) = lines.splitAt(lines.length - 1)
    val numColumns = INDEX_PATTERN.findAllIn(indexesLine).length

    def lineToOptionRow(line: String): Seq[Option[Char]] =
      Range(1, numColumns * 4 + 1, 4).map { index =>
        val char = line.charAt(index)
        if char == ' ' then None else Some(char)
      }

    val initialCargoState = CargoState.empty(numColumns)
    cargoLines.reverseIterator
      .map(lineToOptionRow)
      .foldLeft(initialCargoState)((state, row) => state.appendRow(row))
  }
}
