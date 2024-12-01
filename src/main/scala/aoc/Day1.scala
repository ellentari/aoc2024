package aoc

object Day1 extends App {

  def solvePart1(input: List[(Int, Int)]): Int = {
    val (leftList, rightList) = input.unzip

    leftList.sorted
      .zip(rightList.sorted)
      .map((a, b) => (a - b).abs)
      .sum
  }

  def solvePart2(input: List[(Int, Int)]): Int = {
    val (leftList, rightList) = input.unzip
    val rightListCount = rightList.groupBy(identity).view.mapValues(_.size).toMap

    leftList
      .map(num => num * rightListCount.getOrElse(num, 0))
      .sum
  }

  private def parseLine(line: String) = {
    val parts = line.split("\\s+")

    (parts(0).toInt, parts(1).toInt)
  }

  private val sample =
    """3   4
      |4   3
      |2   5
      |1   3
      |3   9
      |3   3""".stripMargin
      .split("\n")
      .toList
      .map(parseLine)

  private val input = Input.asList("day1.txt")
    .map(parseLine)

  println(solvePart1(sample)) // 11
  println(solvePart1(input)) // 1938424
  println(solvePart2(sample)) // 31
  println(solvePart2(input)) // 22014209

}
