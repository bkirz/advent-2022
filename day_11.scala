import scala.io.Source
import scala.collection.immutable.SortedMap
import scala.math.BigInt

object Day11 {
  sealed trait WorryLevel[Self <: WorryLevel[Self]] {
    def divisibleBy(divisor: Int): Boolean

    def +(other: Int): Self
    def *(other: Int): Self
    def square: Self

    def postProcess: Self
  }

  // Preserves the int behavior from part 1
  case class IntWorryLevel(value: Int) extends WorryLevel[IntWorryLevel] {
    def divisibleBy(divisor: Int): Boolean = value % divisor == 0

    def +(other: Int): IntWorryLevel = IntWorryLevel(value + other)
    def *(other: Int): IntWorryLevel = IntWorryLevel(value * other)
    def square: IntWorryLevel = IntWorryLevel(value * value)

    def postProcess: IntWorryLevel = IntWorryLevel(value / 3)
  }

  // Model worry level as a mapping of prime to its remainder mod that prime
  case class ModWorryLevel(values: Map[Int, Int])
      extends WorryLevel[ModWorryLevel] {

    def divisibleBy(divisor: Int): Boolean = values(divisor) == 0

    def +(other: Int): ModWorryLevel =
      ModWorryLevel(values.map { case (base, value) =>
        (base, (value + other) % base)
      })
    def *(other: Int): ModWorryLevel =
      ModWorryLevel(values.map { case (base, value) =>
        (base, (value * other) % base)
      })
    def square: ModWorryLevel =
      ModWorryLevel(values.map { case (base, value) =>
        (base, (value * value) % base)
      })

    def postProcess: ModWorryLevel = this
  }

  object ModWorryLevel {
    val BASES = List(2, 3, 5, 7, 11, 13, 17, 19) // TODO: Calc from input
    def fromNum(value: Int): ModWorryLevel = {
      ModWorryLevel(BASES.map(base => (base, value % base)).toMap)
    }
  }

  sealed trait Operation { def apply(input: WorryLevel[_]): WorryLevel[_] }
  case class Add(value: Int) extends Operation {
    def apply(input: WorryLevel[_]) = input + value
  }
  case class Mul(value: Int) extends Operation {
    def apply(input: WorryLevel[_]) = input * value
  }
  case object Square extends Operation {
    def apply(input: WorryLevel[_]) = input.square
  }

  case class Monkey(
      items: List[WorryLevel[_]],
      operation: Operation,
      testDivisor: Int,
      ifTrue: Int,
      ifFalse: Int,
      itemsInspected: BigInt
  ) {
    def stepTurn: (Monkey, List[ThrownItem]) = {
      def processItem(item: WorryLevel[_]): ThrownItem = {
        val newWorryLevel = operation(item).postProcess
        val toMonkey: Int =
          if (newWorryLevel.divisibleBy(testDivisor)) { ifTrue }
          else { ifFalse }
        ThrownItem(toMonkey = toMonkey, newWorryLevel = newWorryLevel)
      }

      (
        this.copy(
          items = List.empty,
          itemsInspected = itemsInspected + items.length
        ),
        items.map(processItem)
      )
    }

    def acceptItems(newItems: List[WorryLevel[_]]) =
      this.copy(items = this.items ++ newItems.reverse)
  }

  case class ThrownItem(toMonkey: Int, newWorryLevel: WorryLevel[_])

  case class KeepAwayGame(monkeys: SortedMap[Int, Monkey]) {
    def stepRound: KeepAwayGame = {
      val updatedMap =
        monkeys.keys.foldLeft(monkeys) { case (acc, monkeyNum) =>
          val monkey = acc(monkeyNum)
          val (updatedMonkey, thrownItems) = monkey.stepTurn
          val (trueItems, falseItems) =
            thrownItems.partition(item => item.toMonkey == monkey.ifTrue)

          acc
            .updated(monkeyNum, updatedMonkey)
            .updatedWith(monkey.ifTrue) { maybeMonkey =>
              maybeMonkey.map { m =>
                m.acceptItems(trueItems.map(_.newWorryLevel))
              }
            }
            .updatedWith(monkey.ifFalse) { maybeMonkey =>
              maybeMonkey.map { m =>
                m.acceptItems(falseItems.map(_.newWorryLevel))
              }
            }
        }
      KeepAwayGame(updatedMap)
    }

    def monkeyBusiness: BigInt =
      monkeys.values.map(_.itemsInspected).toArray.sorted.takeRight(2).product
  }

  val PART_1_ROUNDS = 20
  val PART_2_ROUNDS = 10_000

  @main def main = {
    val input = Source.fromFile("day_11.input").getLines().mkString("\n")
    val initialPart1GameState = parseMonkeys(input, (x) => IntWorryLevel(x))
    val finalPart1GameState =
      1.to(PART_1_ROUNDS).foldLeft(initialPart1GameState) { (r, _) =>
        r.stepRound
      }

    println(s"Part 1: ${finalPart1GameState.monkeyBusiness}")

    val initialPart2GameState =
      parseMonkeys(input, (x) => ModWorryLevel.fromNum(x))
    val finalPart2GameState =
      1.to(PART_2_ROUNDS).foldLeft(initialPart2GameState) { (round, roundNum) =>
        if (roundNum % 100 == 0) {
          println(s"Processing round $roundNum with state $round")
        }
        round.stepRound
      }

    println(s"Part 2: ${finalPart2GameState.monkeyBusiness}")
  }

  val PATTERN =
    """Monkey (\d):
      "  Starting items: ([0-9 ,]+)
      "  Operation: new = (old \+ \d+|old \* \d+|old \* old)
      "  Test: divisible by (\d+)
      "    If true: throw to monkey (\d)
      "    If false: throw to monkey (\d)"""
      .stripMargin('"')
      .r
  def parseMonkeys(
      input: String,
      makeWorryLevel: (Int) => WorryLevel[_]
  ): KeepAwayGame = {
    val kvPairs = PATTERN
      .findAllMatchIn(input)
      .map { m =>
        m match {
          case PATTERN(
                num,
                startingItems,
                operation,
                testDivisor,
                ifTrue,
                ifFalse
              ) =>
            (
              num.toInt,
              Monkey(
                items = startingItems
                  .split(", ")
                  .map { str => makeWorryLevel(str.toInt) }
                  .toList,
                operation = parseOperation(operation),
                testDivisor = testDivisor.toInt,
                ifTrue = ifTrue.toInt,
                ifFalse = ifFalse.toInt,
                itemsInspected = 0
              )
            )
        }
      }
    KeepAwayGame(SortedMap.from(kvPairs))
  }

  val ADD_PATTERN = """old \+ (\d+)""".r
  val MUL_PATTERN = """old \* (\d+)""".r
  def parseOperation(opStr: String): Operation = opStr match {
    case ADD_PATTERN(value) => Add(value.toInt)
    case MUL_PATTERN(value) => Mul(value.toInt)
    case "old * old"        => Square
  }
}
