import scala.io.Source

enum Shape {
  case Rock, Paper, Scissors

  def score: Int = ordinal + 1
  def defeats: Shape = Shape.fromOrdinal((ordinal + 2) % 3)
  def defeatedBy: Shape = Shape.fromOrdinal((ordinal + 1) % 3)
}

object Shape {
  def parseOpponent(str: String): Shape =
    str match {
      case "A" => Rock
      case "B" => Paper
      case "C" => Scissors
    }

  def parseYours(str: String): Shape =
    str match {
      case "X" => Rock
      case "Y" => Paper
      case "Z" => Scissors
    }

  def fromOpponentShapeAndOutcome(opponent: Shape, outcome: Outcome) = {
    outcome match {
      case Outcome.Tie  => opponent
      case Outcome.Loss => opponent.defeats
      case Outcome.Win  => opponent.defeatedBy
    }
  }
}

enum Outcome(val score: Int) {
  case Win extends Outcome(6)
  case Tie extends Outcome(3)
  case Loss extends Outcome(0)
}

object Outcome {
  def parse(str: String): Outcome = str match {
    case "X" => Outcome.Loss
    case "Y" => Outcome.Tie
    case "Z" => Outcome.Win
  }
}

case class Round(yours: Shape, opponents: Shape) {
  def score = {
    val outcome = if (yours == opponents) {
      Outcome.Tie
    } else if (yours.defeats == opponents) {
      Outcome.Win
    } else {
      Outcome.Loss
    }
    outcome.score + yours.score
  }
}

object Day02 {
  @main def main() = {
    val lines = Source.fromFile("day_02.input").getLines().toList

    println(
      f"Part 1 (Cumulative score parsing XYZ as shape): ${lines.map(parseLinePart1(_).score).sum}"
    )
    println(
      f"Part 2: (Cumulative score parsing XYZ as desired outcome): ${lines.map(parseLinePart2(_).score).sum}"
    )
  }

  def parseLinePart1(line: String): Round =
    line.split(" ") match {
      case Array(opponent, mine) =>
        Round(Shape.parseYours(mine), Shape.parseOpponent(opponent))
    }

  def parseLinePart2(line: String): Round =
    line.split(" ") match {
      case Array(opponent, outcomeStr) => {
        val opponentShape = Shape.parseOpponent(opponent)
        val desiredOutcome = Outcome.parse(outcomeStr)
        Round(
          Shape.fromOpponentShapeAndOutcome(opponentShape, desiredOutcome),
          opponentShape
        )
      }
    }
}
