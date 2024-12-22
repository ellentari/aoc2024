package aoc

import scala.annotation.tailrec
import scala.collection.mutable

object Day22 extends App {

  private val Simulations = 2000
  private val PriceDiffSequenceLength = 4

  def solvePart1(secrets: List[Int]): Long = secrets.map(getNthSecretNumber(_, Simulations)).sum

  def solvePart2(secrets: List[Int]): Int = {
    @tailrec
    def loop(secrets: List[Int], priceSumBySequence: Map[Vector[Int], Int]): Int =
      secrets match
        case Nil => priceSumBySequence.maxBy(_._2)._2
        case secret :: tail =>
          val prices = calculatePrices(secret, Simulations).toVector
          val sequencePrices = sequencesAndTheirPricesAfter(prices)

          val nextAcc = sequencePrices.foldLeft(priceSumBySequence) { case (acc, (seq, price)) =>
            acc.updatedWith(seq)(sum => Some(sum.getOrElse(0) + price))
          }

          loop(tail, nextAcc)

    loop(secrets, Map.empty)
  }

  private def getNthSecretNumber(initialSecret: Int, n: Int): Long = {
    @tailrec
    def loop(secret: Long, i: Int): Long =
      if (i == n) secret
      else loop(nextSecretNumber(secret), i + 1)

    loop(initialSecret, 0)
  }

  private def nextSecretNumber(secret: Long) = {
    def prune(value: Long) = value % 16777216
    def mix(value1: Long, value2: Long) = value1 ^ value2

    var nextSecret = secret

    nextSecret = prune(mix(nextSecret * 64, nextSecret))
    nextSecret = prune(mix(nextSecret / 32, nextSecret))
    nextSecret = prune(mix(nextSecret * 2048, nextSecret))

    nextSecret
  }

  private def calculatePrices(initialSecret: Int, n: Int): List[Int] = {
    @tailrec
    def loop(secret: Long, i: Int, acc: List[Int]): List[Int] =
      if (i == n) acc.reverse
      else loop(nextSecretNumber(secret), i + 1, calculatePrice(secret) :: acc)

    loop(initialSecret, 0, Nil)
  }

  private def calculatePrice(secret: Long) = (secret % 10).toInt

  private def calculatePriceChanges(prices: Vector[Int]): Vector[Int] =
    (1 until prices.length).view.map(i => prices(i) - prices(i - 1)).toVector

  private def sequencesAndTheirPricesAfter(prices: Vector[Int]) = {
    val priceChanges = (1 until prices.length).view.map(i => prices(i) - prices(i - 1)).toVector

    @tailrec
    def loop(i: Int, slidingWindow: Vector[Int], acc: Map[Vector[Int], Int]): Map[Vector[Int], Int] = {
      if (i == priceChanges.length) acc
      else {
        val nextSlidingWindow = slidingWindow.drop(1) :+ priceChanges(i)
        val priceAfter = prices(i + 1)
        val nextAcc =
          if (!acc.contains(nextSlidingWindow)) acc + (nextSlidingWindow -> priceAfter)
          else acc

        loop(i + 1, nextSlidingWindow, nextAcc)
      }
    }

    val firstSequence = priceChanges.slice(0, PriceDiffSequenceLength)

    loop(PriceDiffSequenceLength, firstSequence, Map(firstSequence -> prices(firstSequence.length)))
  }

  private def parseInput(input: String) = input.split("\n").toList.map(_.toInt)

  private val sample1 = parseInput(
    """1
      |10
      |100
      |2024""".stripMargin
  )

  private val sample2 = parseInput(
    """1
      |2
      |3
      |2024""".stripMargin
  )

  private val input = parseInput(Input.asString("day22.txt"))

  println(solvePart1(sample1)) // 37327623
  println(solvePart1(input)) // 17005483322
  println(solvePart2(sample2)) // 23
  println(solvePart2(input)) // 1910

}
