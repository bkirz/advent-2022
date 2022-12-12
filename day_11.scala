import scala.io.Source
import scala.collection.immutable.SortedMap

object Day11 {
  sealed trait Operation { def apply(input: Int): Int }
  case class Add(value: Int) extends Operation {
    def apply(input: Int) = value + input
  }
  case class Mul(value: Int) extends Operation {
    def apply(input: Int) = value * input
  }
  case object Square extends Operation { def apply(input: Int) = input * input }

  case class Monkey(
      items: List[Int],
      operation: Operation,
      testDivisor: Int,
      ifTrue: Int,
      ifFalse: Int,
      itemsInspected: Int
  ) {
    def stepTurn: (Monkey, List[ThrownItem]) = {
      def processItem(item: Int): ThrownItem = {
        val newWorryLevel = operation(item) / 3
        val toMonkey: Int =
          if (newWorryLevel % testDivisor == 0) { ifTrue }
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

    def acceptItems(newItems: List[Int]) =
      this.copy(items = this.items ++ newItems.reverse)
  }

  case class ThrownItem(toMonkey: Int, newWorryLevel: Int)

  case class KeepAwayGame(monkeys: SortedMap[Int, Monkey]) {
    def stepRound: KeepAwayGame = {
      val updatedMap =
        monkeys.foldLeft(monkeys) { case (acc, (monkeyNum, _monkey)) =>
          val monkey = acc(monkeyNum)
          println(s"Processing monkey turn $monkeyNum, $monkey")
          val (updatedMonkey, thrownItems) = monkey.stepTurn
          println(s"Thrown items: $thrownItems")
          val (trueItems, falseItems) =
            thrownItems.partition(item => item.toMonkey == monkey.ifTrue)

          val innerUpdatedMap: SortedMap[Int, Monkey] =
            acc
              .updated(monkeyNum, updatedMonkey)
              .updatedWith(monkey.ifTrue) { maybeMonkey =>
                println(
                  s"Updating ${monkey.ifTrue} with current value $maybeMonkey"
                )
                maybeMonkey.map { m =>
                  m.acceptItems(trueItems.map(_.newWorryLevel))
                }
              }
              .updatedWith(monkey.ifFalse) { maybeMonkey =>
                println(
                  s"Updating ${monkey.ifFalse} with current value $maybeMonkey"
                )
                maybeMonkey.map { m =>
                  m.acceptItems(falseItems.map(_.newWorryLevel))
                }
              }
          println(innerUpdatedMap)
          innerUpdatedMap
        }
      KeepAwayGame(updatedMap)
    }

    def monkeyBusiness: Int =
      monkeys.values.map(_.itemsInspected).toArray.sorted.takeRight(2).product
  }

  val PART_1_ROUNDS = 20

  @main def main = {
    val input = Source.fromFile("day_11.input").getLines().mkString("\n")
    val initialGameState = parseMonkeys(input)
    val finalGameState =
      1.to(PART_1_ROUNDS).foldLeft(initialGameState) { (round, _) =>
        round.stepRound
      }

    println(s"Part 1: ${finalGameState.monkeyBusiness}")
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
  def parseMonkeys(input: String): KeepAwayGame = {
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
                items = startingItems.split(", ").map(_.toInt).toList,
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
