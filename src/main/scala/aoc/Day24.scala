package aoc

import aoc.Input as ReadInput

import scala.collection.mutable

object Day24 extends App {

  case class Input(initialWireValues: Map[String, Int], gates: List[Gate])

  sealed trait Gate {
    def input1: String
    def input2: String
    def output: String
  }

  object Gate {
    case class AND(input1: String, input2: String, output: String) extends Gate
    case class OR(input1: String, input2: String, output: String) extends Gate
    case class XOR(input1: String, input2: String, output: String) extends Gate
  }

  def solvePart1(input: Input): Long = {
    val computeResult = compute(input)
    val resultBits = getBits(computeResult, "z")

    java.lang.Long.parseLong(resultBits, 2)
  }

  private def compute(input: Input): Map[String, Int] = {
    val gateByOutput = input.gates.map(gate => gate.output -> gate).toMap
    val adjList = input.gates
      .flatMap { gate =>
        List(
          gate.input1 -> gate.output,
          gate.input2 -> gate.output
        )
      }
      .groupMap(_._1)(_._2)

    val computeResult = mutable.HashMap.empty[String, Int]
    val queue = mutable.Queue.empty[String]
    val indegree = mutable.HashMap.empty[String, Int]

    for ((key, values) <- adjList)
      for (value <- values)
        indegree.update(value, indegree.getOrElse(value, 0) + 1)

    input.initialWireValues.keys.foreach(queue.enqueue)

    while (queue.nonEmpty) {
      val wire = queue.dequeue()

      if (input.initialWireValues.contains(wire))
        computeResult.update(wire, input.initialWireValues(wire))
      else {
        val gate = gateByOutput(wire)
        val gateResult = evalGate(gate, computeResult)

        computeResult.update(gate.output, gateResult)
      }

      for (next <- adjList.getOrElse(wire, Nil)) {
        indegree.update(next, indegree(next) - 1)

        if (indegree(next) == 0)
          queue.enqueue(next)
      }
    }

    computeResult.toMap
  }

  private def evalGate(gate: Gate, computeResult: mutable.Map[String, Int]) =
    gate match
      case Gate.AND(input1, input2, output) => computeResult(input1) & computeResult(input2)
      case Gate.OR(input1, input2, output) => computeResult(input1) | computeResult(input2)
      case Gate.XOR(input1, input2, output) => computeResult(input1) ^ computeResult(input2)

  private def getBits(computeResult: Map[String, Int], prefix: String) =
    computeResult.view
      .filter(_._1.startsWith(prefix))
      .toList
      .sortBy(_._1)
      .map(_._2)
      .reverse
      .mkString("")

  private def parseInput(input: String): Input = {
    val parts = input.split("\n\n")

    val initialWireValues = parts(0).split("\n")
      .map {
        case s"$wire: $value" => (wire, value.toInt)
      }
      .toMap

    val gates = parts(1).split("\n")
      .map {
        case s"$input1 $gate $input2 -> $output" => gate match
          case "AND" => Gate.AND(input1, input2, output)
          case "OR" => Gate.OR(input1, input2, output)
          case "XOR" => Gate.XOR(input1, input2, output)
      }
      .toList

    Input(initialWireValues, gates)
  }

  private val sample1 = parseInput(
    """x00: 1
      |x01: 1
      |x02: 1
      |y00: 0
      |y01: 1
      |y02: 0
      |
      |x00 AND y00 -> z00
      |x01 XOR y01 -> z01
      |x02 OR y02 -> z02""".stripMargin
  )

  private val sample2 = parseInput(
    """x00: 1
      |x01: 0
      |x02: 1
      |x03: 1
      |x04: 0
      |y00: 1
      |y01: 1
      |y02: 1
      |y03: 1
      |y04: 1
      |
      |ntg XOR fgs -> mjb
      |y02 OR x01 -> tnw
      |kwq OR kpj -> z05
      |x00 OR x03 -> fst
      |tgd XOR rvg -> z01
      |vdt OR tnw -> bfw
      |bfw AND frj -> z10
      |ffh OR nrd -> bqk
      |y00 AND y03 -> djm
      |y03 OR y00 -> psh
      |bqk OR frj -> z08
      |tnw OR fst -> frj
      |gnj AND tgd -> z11
      |bfw XOR mjb -> z00
      |x03 OR x00 -> vdt
      |gnj AND wpb -> z02
      |x04 AND y00 -> kjc
      |djm OR pbm -> qhw
      |nrd AND vdt -> hwm
      |kjc AND fst -> rvg
      |y04 OR y02 -> fgs
      |y01 AND x02 -> pbm
      |ntg OR kjc -> kwq
      |psh XOR fgs -> tgd
      |qhw XOR tgd -> z09
      |pbm OR djm -> kpj
      |x03 XOR y03 -> ffh
      |x00 XOR y04 -> ntg
      |bfw OR bqk -> z06
      |nrd XOR fgs -> wpb
      |frj XOR qhw -> z04
      |bqk OR frj -> z07
      |y03 OR x01 -> nrd
      |hwm AND bqk -> z03
      |tgd XOR rvg -> z12
      |tnw OR pbm -> gnj""".stripMargin
  )

  private val sample3 =
    """x00: 0
      |x01: 1
      |x02: 0
      |x03: 1
      |x04: 0
      |x05: 1
      |y00: 0
      |y01: 0
      |y02: 1
      |y03: 1
      |y04: 0
      |y05: 1
      |
      |x00 AND y00 -> z05
      |x01 AND y01 -> z02
      |x02 AND y02 -> z01
      |x03 AND y03 -> z03
      |x04 AND y04 -> z04
      |x05 AND y05 -> z00""".stripMargin

  private val input = parseInput(ReadInput.asString("day24.txt"))

  println(solvePart1(sample1)) // 4
  println(solvePart1(sample2)) // 2024
  println(solvePart1(input)) // 65740327379952

}