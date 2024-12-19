package aoc

import aoc.Input as ReadInput

import scala.collection.mutable

object Day19 extends App {

  case class Input(availablePatterns: List[String], desiredDesign: List[String])

  def solvePart1(input: Input): Int =
    input.desiredDesign
      .count(canMake(input.availablePatterns))

  def solvePart2(input: Input): Long =
    input.desiredDesign
      .map(countWaysToMake(input.availablePatterns))
      .sum

  private def canMake(availablePatterns: List[String])(desiredDesign: String): Boolean = {

    def loop(start: Int): Boolean =
      if (start == desiredDesign.length) true
      else
        availablePatterns
          .exists(pattern => desiredDesign.startsWith(pattern, start) && loop(start + pattern.length))

    loop(0)
  }

  private def countWaysToMake(availablePatterns: List[String])(desiredDesign: String): Long = {

    val memo = mutable.HashMap.empty[Int, Long]

    def loop(start: Int): Long = {
      if (start == desiredDesign.length) 1
      else if (memo.contains(start)) memo(start)
      else
        val result = availablePatterns
          .filter(desiredDesign.startsWith(_, start))
          .map(pattern => loop(start + pattern.length))
          .sum

        memo.update(start, result)

        result
    }

    loop(0)
  }

  private def parseInput(input: String): Input = {
    val parts = input.split("\n\n")

    val availablePatterns = parts(0).split(", ").toList
    val desiredDesigned = parts(1).split("\n").toList

    Input(availablePatterns, desiredDesigned)
  }

  private val sample = parseInput(
    """r, wr, b, g, bwu, rb, gb, br
      |
      |brwrr
      |bggr
      |gbbr
      |rrbgbr
      |ubwu
      |bwurrg
      |brgr
      |bbrgwb""".stripMargin
  )

  private val input = parseInput(ReadInput.asString("day19.txt"))

  println(solvePart1(sample)) // 6
  println(solvePart1(input)) // 228
  println(solvePart2(sample)) // 16
  println(solvePart2(input)) // 584553405070389

}
