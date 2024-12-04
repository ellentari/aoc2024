package aoc

import aoc.Day3.Token._

import scala.annotation.tailrec

object Day3 extends App {

  sealed trait Token

  object Token:
    case object Do extends Token
    case object Dont extends Token
    case class Mul(x: Int, y: Int) extends Token:
      def multiply: Int = x * y

  private val parsePart1: String => List[Mul] = parseTokens(parseMul)
  private val parsePart2: String => List[Token] = parseTokens(parseAnyToken)

  def solvePart1(input: String): Int = eval(parsePart1(input))

  def solvePart2(input: String): Int = eval(parsePart2(input))

  private def eval(tokens: List[Token]) = {
    @tailrec
    def loop(tokens: List[Token], mulEnabled: Boolean, acc: Int): Int =
      tokens match
        case Nil => acc
        case Do :: tail => loop(tail, mulEnabled = true, acc)
        case Dont :: tail => loop(tail, mulEnabled = false, acc)
        case (mul: Mul) :: tail => loop(tail, mulEnabled, if (mulEnabled) acc + mul.multiply else acc)

    loop(tokens, mulEnabled = true, acc = 0)
  }

  private def parseTokens[A](doParse: (String, Int) => Option[(A, Int)])(s: String) = {
    @tailrec
    def loop(start: Int, acc: List[A]): List[A] =
      if (start >= s.length) acc.reverse
      else
        doParse(s, start) match
          case None => loop(start + 1, acc)
          case Some((token, nextStart)) =>
            loop(nextStart, token :: acc)

    loop(0, Nil)
  }

  private def parseAnyToken(s: String, start: Int): Option[(Token, Int)] =
    parseDo(s, start)
      .orElse(parseDont(s, start))
      .orElse(parseMul(s, start))

  private def parseMul(s: String, start: Int): Option[(Mul, Int)] =
    if (s.startsWith("mul(", start))
      for {
        (x, end1) <- parseInt(s, start + 4) if end1 < s.length && s(end1) == ','
        (y, end2) <- parseInt(s, end1 + 1) if end2 < s.length && s(end2) == ')'
      } yield (Mul(x, y), end2 + 1)
    else
      None

  private def parseDo = parseConst("do()", Do)

  private def parseDont = parseConst("don't()", Dont)

  private def parseConst[A](value: String, a: => A)(s: String, start: Int): Option[(A, Int)] =
    Option.when(s.startsWith(value, start))((a, start + value.length))

  private def parseInt(s: String, start: Int): Option[(Int, Int)] = {
    @tailrec
    def loop(i: Int, acc: Int): (Int, Int) =
      if (i < s.length && s(i).isDigit) loop(i + 1, acc * 10 + (s(i) - '0'))
      else (acc, i)

    val (result, end) = loop(start, 0)

    Option.unless(result == 0)((result, end))
  }

  private val sample1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  private val sample2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

  private val input = Input.asString("day3.txt")

  println(solvePart1(sample1)) // 161
  println(solvePart1(input)) // 178794710
  println(solvePart2(sample2)) // 48
  println(solvePart2(input)) // 76729637

}
