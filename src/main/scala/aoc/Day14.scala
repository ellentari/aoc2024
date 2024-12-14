package aoc

import scala.annotation.tailrec

object Day14 extends App {

  case class Robot(x: Int, y: Int, vx: Int, vy: Int)

  def solvePart1(initial: List[Robot], height: Int, width: Int): Int = {

    def getQuadrant(robot: Robot): Option[Int] =
      if (robot.x < width / 2 && robot.y < height / 2) Some(1)
      else if (robot.x > width / 2 && robot.y < height / 2) Some(2)
      else if (robot.x < width / 2 && robot.y > height / 2) Some(3)
      else if (robot.x > width / 2 && robot.y > height / 2) Some(4)
      else None

    val finalPositions = (0 until 100).foldLeft(initial)((robots, _) => robots.map(move(_, height, width)))

    finalPositions
      .flatMap(getQuadrant)
      .groupBy(identity)
      .view
      .mapValues(_.length)
      .values
      .product
  }

  def solvePart2(initial: List[Robot], height: Int, width: Int): (List[Robot], Int) = {

    @tailrec
    def loop(robots: List[Robot], round: Int): (List[Robot], Int) = {
      val nextRobots = robots.map(move(_, height, width))

      if (containsVerticalLine(nextRobots, 10)) (nextRobots, round)
      else loop(nextRobots, round + 1)
    }

    loop(initial, round = 1)
  }

  private def move(robot: Robot, height: Int, width: Int) =
    robot.copy(
      x = wrap(robot.x, robot.vx, width),
      y = wrap(robot.y, robot.vy, height)
    )

  private def containsVerticalLine(robots: List[Robot], atLeast: Int): Boolean = {
    val byX = robots.groupBy(_.x).view.mapValues(_.sortBy(_.y)).toMap

    byX.exists((_, v) => atLeast <= getLongestConsecutiveRowsCount(v))
  }

  private def getLongestConsecutiveRowsCount(robots: List[Robot]): Int = {
    @tailrec
    def loop(robots: List[Robot], prevY: Int, currentLength: Int, maxLength: Int): Int =
      robots match
        case Nil => maxLength
        case head :: tail =>
          if (head.y == prevY + 1) loop(tail, head.y, currentLength + 1, maxLength max (currentLength + 1))
          else loop(tail, head.y, 1, maxLength max 1)

    loop(robots, -1, 0, 0)
  }

  private def wrap(i: Int, k: Int, size: Int) = {
    val j = (i + k) % size

    if (j < 0) (j + size) % size
    else j
  }

  private def draw(robots: List[Robot], height: Int, width: Int) = {
    val canvas = Array.fill[Char](height, width)('.')

    for (robot <- robots)
      canvas(robot.y)(robot.x) = '#'

    canvas.map(_.mkString("")).mkString("\n")
  }

  private def parseRobot(s: String) = s match
    case s"p=$x,$y v=$vx,$vy" => Robot(x.toInt, y.toInt, vx.toInt, vy.toInt)

  private val sample =
    """p=0,4 v=3,-3
      |p=6,3 v=-1,-3
      |p=10,3 v=-1,2
      |p=2,0 v=2,-1
      |p=0,0 v=1,3
      |p=3,0 v=-2,-2
      |p=7,6 v=-1,-3
      |p=3,0 v=-1,-2
      |p=9,3 v=2,3
      |p=7,3 v=-1,2
      |p=2,4 v=2,-3
      |p=9,5 v=-3,-3""".stripMargin
      .split("\n")
      .map(parseRobot)
      .toList

  private val input = Input.asList("day14.txt").map(parseRobot)
  private val inputHeight = 103
  private val inputWidth = 101

  println(solvePart1(sample, 7, 11)) // 12
  println(solvePart1(input, inputHeight, inputWidth)) // 228410028

  val (part2Result, part2Rounds) = solvePart2(input, inputHeight, inputWidth)
  println(part2Rounds) // 8258
  println(draw(part2Result, inputHeight, inputWidth))

}
