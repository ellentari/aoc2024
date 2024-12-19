package aoc

import aoc.Day17.Register.*

import scala.annotation.tailrec
import scala.collection.mutable

object Day17 extends App {

  enum Register:
    case A, B, C

  sealed trait Operand
  object Operand {
    sealed trait Combo extends Operand

    case class Literal(value: Int) extends Combo
    case class RegisterValue(register: Register) extends Combo
  }

  sealed trait ProgramInstruction
  object ProgramInstruction {
    case class Division(operand: Operand.Combo, storeTo: Register) extends ProgramInstruction
    case class XOR(operand: Operand.Combo) extends ProgramInstruction
    case class Mod(operand: Operand.Combo) extends ProgramInstruction
    case class Jump(operand: Operand.Literal) extends ProgramInstruction
    case class Out(operand: Operand.Combo) extends ProgramInstruction
  }

  case class Program(
    initialRegisters: Map[Register, Long],
    instructions: Vector[ProgramInstruction],
    originalInstructions: List[Int])

  case class ProgramState(pointer: Int, registers: Map[Register, Long], reverseOutput: List[Int]) {

    def output: List[Int] = reverseOutput.reverse

    def register(register: Register): Long = registers(register)

    def store(register: Register, value: Long): ProgramState =
      copy(registers = registers.updated(register, value))

    def nextPointer: ProgramState = copy(pointer = pointer + 1)

    def jumpTo(pointer: Int): ProgramState = copy(pointer = pointer)

    def output(value: Int): ProgramState =
      copy(reverseOutput = value :: reverseOutput)

  }

  def solvePart1(program: Program): String =
    runProgram(program).mkString(",")

  def solvePart2(): Long = {
    @tailrec
    def search(nextAs: List[Long], offset: Int): List[Long] = {
      if (offset < 0) nextAs
      else {
        val expectedOutput = input.originalInstructions.drop(offset)
        val validAs = for {
          nextA <- nextAs
          i <- 0 until 8
          prevA = nextA * 8 + i
          output = runInputProgram(prevA) if output == expectedOutput
        } yield prevA

        search(validAs, offset - 1)
      }
    }

    search(List(0L), input.originalInstructions.size - 1)
      .min
  }

  private def runProgram(program: Program): List[Int] = {

    @tailrec
    def loop(state: ProgramState): ProgramState =
      if (state.pointer >= program.instructions.size) state
      else loop(executeInstruction(program.instructions(state.pointer), state))

    val initialState = ProgramState(0, program.initialRegisters, Nil)
    val finalState = loop(initialState)

    finalState.output
  }

  private def executeInstruction(instruction: ProgramInstruction, state: ProgramState): ProgramState =
    instruction match
      case ProgramInstruction.Division(operand, register) =>
        val numerator = state.register(Register.A)
        val denominator = scala.math.pow(2, comboOperandValue(operand, state)).toLong
        val result = numerator / denominator

        state
          .store(register, result)
          .nextPointer

      case ProgramInstruction.XOR(operand) =>
        val result = state.register(Register.B) ^ comboOperandValue(operand, state)

        state
          .store(Register.B, result)
          .nextPointer

      case ProgramInstruction.Mod(operand) =>
        val result = comboOperandValue(operand, state) % 8

        state
          .store(Register.B, result)
          .nextPointer

      case ProgramInstruction.Jump(operand) =>
        if (state.register(Register.A) != 0) state.jumpTo(operand.value)
        else state.nextPointer

      case ProgramInstruction.Out(operand) =>
        val result = (comboOperandValue(operand, state) % 8).toInt

        state
          .output(result)
          .nextPointer

  private def comboOperandValue(operand: Operand.Combo, state: ProgramState): Long = operand match
    case Operand.Literal(value) => value
    case Operand.RegisterValue(register) => state.register(register)

  // this is what input program looks like
  private def runInputProgram(initA: Long): List[Int] = {
    var a = initA

    val output = mutable.ListBuffer.empty[Int]

    while (a != 0) {
      var b = a % 8
      b = b ^ 1
      val c = a / scala.math.pow(2, b).toLong
      b = b ^ 5
      b = b ^ c

      output.append((b % 8).toInt)

      a = a / 8
    }

    output.toList
  }

  private def parseProgram(input: String): Program = {

    def parseRegisterValue(register: String) = register.split(": ")(1).toLong

    def comboOperand(value: Int): Operand.Combo = value match
      case v if v >= 0 && v <= 3 => Operand.Literal(v)
      case 4 => Operand.RegisterValue(Register.A)
      case 5 => Operand.RegisterValue(Register.B)
      case 6 => Operand.RegisterValue(Register.C)

    val parts = input.split("\n\n")

    val registers = parts(0).split("\n")
    val a = parseRegisterValue(registers(0))
    val b = parseRegisterValue(registers(1))
    val c = parseRegisterValue(registers(2))

    val program = parts(1).split(": ")(1).split(",").map(_.toInt).toList

    val instructions = program.grouped(2)
      .map {
        case List(opCode, operand) =>
          opCode match
            case 0 => ProgramInstruction.Division(comboOperand(operand), Register.A)
            case 1 => ProgramInstruction.XOR(Operand.Literal(operand))
            case 2 => ProgramInstruction.Mod(comboOperand(operand))
            case 3 => ProgramInstruction.Jump(Operand.Literal(operand / 2)) // each final instruction will have index = original index / 2
            case 4 => ProgramInstruction.XOR(Operand.RegisterValue(Register.C))
            case 5 => ProgramInstruction.Out(comboOperand(operand))
            case 6 => ProgramInstruction.Division(comboOperand(operand), Register.B)
            case 7 => ProgramInstruction.Division(comboOperand(operand), Register.C)
      }
      .toVector

    Program(Map(A -> a, B -> b, C -> c), instructions, program)
  }

  private val sample1 = parseProgram(
    """Register A: 729
      |Register B: 0
      |Register C: 0
      |
      |Program: 0,1,5,4,3,0""".stripMargin
  )

  private val input = parseProgram(Input.asString("day17.txt"))

  println(solvePart1(sample1)) // 4,6,3,5,6,3,5,2,1,0
  println(solvePart1(input)) // 7,5,4,3,4,5,3,4,6
  println(solvePart2()) // 164278899142333

}
