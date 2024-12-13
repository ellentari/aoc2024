package aoc

object Day13 extends App {

  case class Configuration(ax: Int, ay: Int, bx: Int, by: Int, tx: Long, ty: Long) {
    def addToTarget(toAdd: Long): Configuration =
      copy(tx = tx + toAdd, ty = ty + toAdd)
  }

  case class EquationResult(a: Long, b: Long)

  def solvePart1(input: List[Configuration]): Long = solve(input)

  def solvePart2(input: List[Configuration]): Long = solve(input.map(_.addToTarget(10000000000000L)))

  private def solve(input: List[Configuration]): Long =
    input
      .flatMap(solveEquations)
      .map(r => r.a * 3 + r.b)
      .sum

  // ax * a + bx * b = tx
  // ay * a + by * b = ty
  //
  // if:
  // a = (tx - bx * b) / ax
  // then:
  // ay * (tx - bx * b) / ax + by * b = ty
  // b = (ax * ty - ay * tx) / (ax * by - ay * bx)
  //
  // if:
  // b = (tx - ax * a) / bx
  // then:
  // ay * a + by * (tx - ax * a) / bx = ty
  // a = (bx * ty - by * tx) / (ay * bx - ax * by)
  //
  // finally:
  // a = (bx * ty - by * tx) / (ay * bx - ax * by)
  // b = (ax * ty - ay * tx) / (ax * by - ay * bx)
  private def solveEquations(configuration: Configuration): Option[EquationResult] = {
    import configuration.*

    val a = (bx * ty - by * tx) / (ay * bx - ax * by)
    val b = (ax * ty - ay * tx) / (ax * by - ay * bx)

    Option.when(ax * a + bx * b == tx && ay * a + by * b == ty)(EquationResult(a, b))
  }

  private def parseConfigurations(s: String): List[Configuration] =
    s.split("\n\n").map(parseConfiguration).toList

  private def parseConfiguration(s: String): Configuration = {
    val lines = s.split("\n")

    val (ax, ay) = lines(0) match
      case s"Button A: X$ax, Y$ay" => (ax.toInt, ay.toInt)
    val (bx, by) = lines(1) match
      case s"Button B: X$bx, Y$by" => (bx.toInt, by.toInt)
    val (tx, ty) = lines(2) match
      case s"Prize: X=$x, Y=$y" => (x.toLong, y.toLong)

    Configuration(ax, ay, bx, by, tx, ty)
  }

  private val sample = parseConfigurations(
    """Button A: X+94, Y+34
      |Button B: X+22, Y+67
      |Prize: X=8400, Y=5400
      |
      |Button A: X+26, Y+66
      |Button B: X+67, Y+21
      |Prize: X=12748, Y=12176
      |
      |Button A: X+17, Y+86
      |Button B: X+84, Y+37
      |Prize: X=7870, Y=6450
      |
      |Button A: X+69, Y+23
      |Button B: X+27, Y+71
      |Prize: X=18641, Y=10279""".stripMargin
  )

  private val input = parseConfigurations(Input.asString("day13.txt"))

  println(solvePart1(sample)) // 480
  println(solvePart1(input)) // 25751
  println(solvePart2(sample)) // 875318608908
  println(solvePart2(input)) // 108528956728655

}
