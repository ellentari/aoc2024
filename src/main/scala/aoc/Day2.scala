package aoc

import scala.annotation.tailrec

object Day2 extends App {

  private val MaxDiff = 3

  def solvePart1(input: List[Vector[Int]]) = input.count(isSafe)

  def solvePart2(input: List[Vector[Int]]) = input.count(isSafeWithTolerance)

  private def isSafe(report: Vector[Int]): Boolean = {
    val pairs = report.sliding(2).map(pair => pair(0) -> pair(1)).toList

    val (a, b) = pairs.head
    val isIncreasing = a < b

    @tailrec
    def loop(pairs: List[(Int, Int)]): Boolean =
      pairs match
        case Nil => true
        case (a, b) :: tail =>
          def correctDiff = (a - b).abs <= MaxDiff
          def correctOrder = if (isIncreasing) a < b else a > b

          correctDiff && correctOrder && loop(tail)

    loop(pairs)
  }

  private def isSafeWithTolerance(report: Vector[Int]): Boolean =
    isSafe(report) || report.indices.view.map(i => report.patch(i, Nil, 1)).exists(isSafe)

  private val sample =
    """7 6 4 2 1
      |1 2 7 8 9
      |9 7 6 2 1
      |1 3 2 4 5
      |8 6 4 4 1
      |1 3 6 7 9""".stripMargin
      .split("\n")
      .toList
      .map(_.split(" ").map(_.toInt).toVector)

  private val input = Input.asList("day2.txt")
    .map(_.split(" ").map(_.toInt).toVector)

  println(solvePart1(sample)) // 2
  println(solvePart1(input)) // 463
  println(solvePart2(sample)) // 4
  println(solvePart2(input)) // 514

}
