import scala.io.Source
import scala.util.Try

enum Shape(val score: Int) {
  case Rock extends Shape(1)
  case Paper extends Shape(2)
  case Scissors extends Shape(3)
}

object Shape {
  def parse(str: String): Shape =
    str match {
      case "A" | "X" => Rock
      case "B" | "Y" => Paper
      case "C" | "Z" => Scissors
    }
}

enum Outcome(val score: Int) {
  case Win extends Outcome(6)
  case Tie extends Outcome(3)
  case Loss extends Outcome(0)
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

object Day02 {
  @main def main() = {
    val rounds = Source
      .fromFile("day_02.input")
      .getLines()
      .map(parseLine)

    println(
      f"Part 1 (Cumulative score of all rounds): ${rounds.map(_.score).sum}"
    )
  }

  def parseLine(line: String): Round =
    line.split(" ") match {
      case Array(opponent, mine) =>
        Round(Shape.parse(mine), Shape.parse(opponent))
    }
}
