package aoc

import aoc.util.Grid

object Day21 extends App {

  opaque type NumericCode = String
  opaque type DirectionalCode = String

  private val ApplyButton = 'A'
  private val UpButton = '^'
  private val DownButton = 'v'
  private val RightButton = '>'
  private val LeftButton = '<'

  private val numericKeypad = Grid.parseCharacterGrid(
    """789
      |456
      |123
      | 0A""".stripMargin
  )
  private val numericKeypadTranslationTable: Map[(Char, Char), List[DirectionalCode]] =
    getTranslationTableForKeypad(numericKeypad, numericKeypad.bottomLeftCorner)

  private val directionalKeypad = Grid.parseCharacterGrid(
    """ ^A
      |<v>""".stripMargin
  )
  private val directionalKeypadTranslationTable: Map[(Char, Char), List[DirectionalCode]] =
    getTranslationTableForKeypad(directionalKeypad, directionalKeypad.topLeftCorner)

  private val cache = scala.collection.mutable.HashMap.empty[(Char, Char, Int), Long]

  def solve(numericCodes: List[NumericCode], intermediateRobots: Int): Long =
    numericCodes.map(calculateComplexity(intermediateRobots)).sum

  private def getTranslationTableForKeypad[A](keypad: Grid[A], void: Grid.Index) =
    (for {
      from <- keypad.indices if from != void
      to <- keypad.indices if to != void
    } yield keypad(from) -> keypad(to) -> translate(from, to, void))
      .toMap

  private def translate(from: Grid.Index, to: Grid.Index, void: Grid.Index): List[DirectionalCode] = {

    def mkDirectionalCode(firstMove: Char, firstMoveCount: Int, secondMove: Char, secondMoveCount: Int): DirectionalCode =
      new StringBuilder()
        .appendAll(Iterator.fill(firstMoveCount)(firstMove))
        .appendAll(Iterator.fill(secondMoveCount)(secondMove))
        .append(ApplyButton)
        .toString()

    val rowDiff = from.row - to.row
    val colDiff = from.column - to.column

    val verticalMove = if (rowDiff > 0) UpButton else DownButton
    val horizontalMove = if (colDiff > 0) LeftButton else RightButton

    def verticalThenHorizontal = mkDirectionalCode(verticalMove, rowDiff.abs, horizontalMove, colDiff.abs)

    def horizontalThenVertical = mkDirectionalCode(horizontalMove, colDiff.abs, verticalMove, rowDiff.abs)

    val willGoOverVoid1 = from.row == void.row && to.column == void.column
    val willGoOverVoid2 = from.column == void.column && to.row == void.row

    if (willGoOverVoid1) List(verticalThenHorizontal)
    else if (willGoOverVoid2) List(horizontalThenVertical)
    else if (rowDiff == 0 || colDiff == 0) List(verticalThenHorizontal)
    else List(verticalThenHorizontal, horizontalThenVertical)
  }

  private def calculateComplexity(intermediateRobots: Int)(code: NumericCode) = {
    val directionalInputLength = translateNumericCode(code)
      .map(translateMinLengthDirectionalCode(_, intermediateRobots))
      .min

    directionalInputLength * code.init.toInt
  }

  private def translateNumericCode(numericCode: NumericCode): List[DirectionalCode] =
    (ApplyButton +: numericCode)
      .sliding(2)
      .foldLeft(List(""))((accs, pair) =>
        for {
          acc <- accs
          next <- numericKeypadTranslationTable(pair(0) -> pair(1))
        } yield acc + next
      )

  private def translateMinLengthDirectionalCode(code: DirectionalCode, times: Int): Long = {

    def translateRec(from: Char, to: Char, times: Int): Long = {
      if (times == 1)
        directionalKeypadTranslationTable(from -> to).map(_.length).min
      else if (cache.contains((from, to, times)))
        cache((from, to, times))
      else {
        val minResult = directionalKeypadTranslationTable(from -> to)
          .map(translateMinLengthDirectionalCode(_, times - 1))
          .min

        cache.update((from, to, times), minResult)

        minResult
      }
    }

    (ApplyButton +: code)
      .sliding(2)
      .map(pair => translateRec(pair(0), pair(1), times))
      .sum
  }

  private val sample: List[NumericCode] =
    """029A
      |980A
      |179A
      |456A
      |379A""".stripMargin.split("\n").toList

  private val input: List[NumericCode] = Input.asList("day21.txt")

  println(solve(sample, intermediateRobots = 2)) // 126384
  println(solve(input, intermediateRobots = 2)) // 212488

  cache.clear()

  println(solve(input, intermediateRobots = 25)) // 258263972600402

}
