import scala.io.Source
object Day10 {
  sealed trait Instruction {
    val cycleCount: Int;
    def apply(registers: Registers): Registers;
  }
  case object Noop extends Instruction {
    val cycleCount = 1
    def apply(registers: Registers): Registers = registers
  }
  case class AddX(value: Int) extends Instruction {
    val cycleCount = 2
    def apply(registers: Registers): Registers =
      registers.copy(x = registers.x + value)
  }

  case class Registers(x: Int)
  case class CpuState(
      cycleNum: Int,
      registers: Registers,
      instructions: List[Instruction],
      cyclesIntoCurrentInstruction: Int
  ) {
    def step: CpuState = {
      if (isComplete) { return this }
      val currentInstruction :: nextInstructions = instructions

      if (cyclesIntoCurrentInstruction + 1 == currentInstruction.cycleCount) {
        this.copy(
          cycleNum = cycleNum + 1,
          instructions = nextInstructions,
          registers = currentInstruction.apply(registers),
          cyclesIntoCurrentInstruction = 0
        )
      } else {
        this.copy(
          cycleNum = cycleNum + 1,
          cyclesIntoCurrentInstruction = cyclesIntoCurrentInstruction + 1
        )
      }
    }

    def isComplete: Boolean = instructions.isEmpty

    def signalStrength = cycleNum * registers.x

    override def toString(): String = {
      s"CpuState($cycleNum, $cyclesIntoCurrentInstruction, $registers, current_instruction: ${instructions.head}, remaining_instructions: ${instructions.length})"
    }
  }

  @main def main = {
    val lines = Source.fromFile("day_10.input").getLines().toList
    val instructions = lines.map(parseLine)
    val initialCpuState = CpuState(
      cycleNum = 1,
      registers = Registers(x = 1),
      instructions = instructions,
      cyclesIntoCurrentInstruction = 0
    )
    val checkSignalStrengthAt = List(20, 60, 100, 140, 180, 220)

    val stateIterator: Stream[CpuState] =
      Stream.iterate(initialCpuState)(_.step).tapEach(println(_))

    val part1 = checkSignalStrengthAt.map { cycleNum =>
      stateIterator.find(_.cycleNum == cycleNum).get.signalStrength
    }.sum

    println(s"Part 1: ${part1}")
  }

  val ADDX_PATTERN = s"""addx (-?\\d+)""".r
  def parseLine(line: String): Instruction = {
    line match {
      case "noop"              => Noop
      case ADDX_PATTERN(value) => AddX(value.toInt)
    }
  }
}
