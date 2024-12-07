package aoc

import aoc.Day7.Operator.*

object Day7 extends App {

  case class Equation(result: Long, operands: List[Long])

  enum Operator(val eval: (Long, Long) => Long):
    case Add extends Operator(_ + _)
    case Multiply extends Operator(_ * _)
    case Concat extends Operator((left, right) => (left.toString + right).toLong)

  def solvePart1: List[Equation] => Long = solve(List(Add, Multiply))

  def solvePart2: List[Equation] => Long = solve(List(Add, Multiply, Concat))

  private def solve(operators: List[Operator])(equations: List[Equation]): Long =
    equations
      .filter(canBeTrue(operators))
      .map(_.result)
      .sum

  private def canBeTrue(operators: List[Operator])(equation: Equation): Boolean = {
    def loop(current: Long, operands: List[Long]): Boolean =
      operands match
        case Nil => current == equation.result
        case head :: tail => operators.exists(op => loop(op.eval(current, head), tail))

    loop(equation.operands.head, equation.operands.tail)
  }

  private def parseEquation(s: String): Equation = s match
    case s"$res: $op" =>
      val result = res.toLong
      val operands = op.split(" ").map(_.toLong).toList
      Equation(result, operands)

  private val sample =
    """190: 10 19
      |3267: 81 40 27
      |83: 17 5
      |156: 15 6
      |7290: 6 8 6 15
      |161011: 16 10 13
      |192: 17 8 14
      |21037: 9 7 18 13
      |292: 11 6 16 20""".stripMargin
      .split("\n")
      .map(parseEquation)
      .toList

  private val input = Input.asList("day7.txt").map(parseEquation)

  println(solvePart1(sample)) // 3749
  println(solvePart1(input)) // 14711933466277
  println(solvePart2(sample)) // 11387
  println(solvePart2(input)) // 286580387663654

}
