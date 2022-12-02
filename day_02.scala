import scala.io.Source
import scala.util.Try

enum Shape(val score: Int) {
  case Rock extends Shape(1)
  case Paper extends Shape(2)
  case Scissors extends Shape(3)
}

object Shape {
  def parseOpponent(str: String): Shape =
    str match {
      case "A" => Rock
      case "B" => Paper
      case "C" => Scissors
    }

  def parseMinePart1(str: String): Shape =
    str match {
      case "X" => Rock
      case "Y" => Paper
      case "Z" => Scissors
    }

  def fromOpponentShapeAndOutcome(opponent: Shape, outcome: Outcome) = {
    (opponent, outcome) match {
      case (_, Outcome.Tie)               => opponent
      case (Shape.Rock, Outcome.Win)      => Shape.Paper
      case (Shape.Paper, Outcome.Win)     => Shape.Scissors
      case (Shape.Scissors, Outcome.Win)  => Shape.Rock
      case (Shape.Rock, Outcome.Loss)     => Shape.Scissors
      case (Shape.Paper, Outcome.Loss)    => Shape.Rock
      case (Shape.Scissors, Outcome.Loss) => Shape.Paper
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
    val outcome: Outcome = this match {
      case Round(Shape.Rock, Shape.Scissors)     => Outcome.Win
      case Round(Shape.Paper, Shape.Rock)        => Outcome.Win
      case Round(Shape.Scissors, Shape.Paper)    => Outcome.Win
      case Round(Shape.Rock, Shape.Rock)         => Outcome.Tie
      case Round(Shape.Paper, Shape.Paper)       => Outcome.Tie
      case Round(Shape.Scissors, Shape.Scissors) => Outcome.Tie
      case Round(Shape.Rock, Shape.Paper)        => Outcome.Loss
      case Round(Shape.Paper, Shape.Scissors)    => Outcome.Loss
      case Round(Shape.Scissors, Shape.Rock)     => Outcome.Loss
    }
    outcome.score + yours.score
  }
}

object Round {}

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
        Round(Shape.parseMinePart1(mine), Shape.parseOpponent(opponent))
    }

  def parseLinePart2(line: String): Round =
    line.split(" ") match {
      case Array(opponent, outcomeStr) => {
        val opponentShape = Shape.parseOpponent(opponent)
        val outcome = Outcome.parse(outcomeStr)
        Round(
          Shape.fromOpponentShapeAndOutcome(opponentShape, outcome),
          opponentShape
        )
      }
    }
}
