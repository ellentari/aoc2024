package aoc

object Day11 extends App {

  def solve(input: List[Long], rounds: Int): Long = {
    val uniqueCount = input.groupBy(identity).view.mapValues(_.size.toLong).toMap
    val result = evolve(uniqueCount, rounds)

    result.values.sum
  }

  private def evolve(initial: Map[Long, Long], rounds: Int): Map[Long, Long] =
    (0 until rounds).foldLeft(initial)((acc, _) => evolve(acc))

  private def evolve(numbers: Map[Long, Long]): Map[Long, Long] = {
    val result = scala.collection.mutable.HashMap.empty[Long, Long]

    for ((num, count) <- numbers)
      for (next <- transformNumber(num))
        result.updateWith(next)(c => Some(c.getOrElse(0L) + count))

    result.toMap
  }

  private def transformNumber(number: Long) = {
    val numString = number.toString

    number match
      case 0 => List(1L)
      case _ if numString.length % 2 == 0 =>
        val (left, right) = split(numString)

        List(left, right)
      case other => List(other * 2024)
  }

  private def split(numString: String) = {
    val (leftPart, rightPart) = numString.splitAt(numString.length / 2)

    (leftPart.toLong, rightPart.toLong)
  }

  private def parseInput(s: String) = s.split(" ").map(_.toLong).toList

  private val sample = parseInput("125 17")
  private val input = parseInput(Input.asString("day11.txt"))

  println(solve(sample, rounds = 25)) // 55312
  println(solve(input, rounds = 25)) // 189167
  println(solve(input, rounds = 75)) // 225253278506288

}
