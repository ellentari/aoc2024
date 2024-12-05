package aoc

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day5 extends App {

  opaque type Page = Int

  case class OrderingRule(first: Page, second: Page)

  case class Update(pages: Vector[Page]) {
    def apply(i: Int): Page = pages(i)
    def middlePage: Page = pages(pages.length / 2)
  }

  def solvePart1(orderingRules: List[OrderingRule], updates: List[Update]): Int = {
    val orderMap = orderingRules.groupMap(_.first)(_.second)

    updates
      .filter(isOrdered(_, orderMap))
      .map(_.middlePage)
      .sum
  }

  def solvePart2(orderingRules: List[OrderingRule], updates: List[Update]): Int = {
    val orderMap = orderingRules.groupMap(_.first)(_.second)

    updates
      .filterNot(isOrdered(_, orderMap))
      .map(sortTopologically(_, orderMap))
      .map(_.middlePage)
      .sum
  }

  private def isOrdered(update: Update, order: Map[Page, List[Page]]): Boolean =
    (for {
      i <- update.pages.indices
      first = update(i)
      j <- i + 1 until update.pages.length
      second = update(j)
      isOutOfOrder = order.getOrElse(second, Nil).contains(first)
    } yield !isOutOfOrder).forall(identity)

  private def sortTopologically(update: Update, order: Map[Page, List[Page]]): Update = {
    val relevantOrder = order.view
      .filterKeys(update.pages.contains)
      .mapValues(_.filter(update.pages.contains))
      .toMap

    @tailrec
    def loop(queue: Queue[Page], inDegree: Map[Page, Int], acc: List[Page]): Update = {
      queue.dequeueOption match
        case None => Update(acc.reverse.toVector)

        case Some((page, remainingQueue)) =>
          val next = relevantOrder.getOrElse(page, Nil)
          val updInDegree = next.foldLeft(inDegree)(_.updatedWith(_)(_.map(_ - 1)))
          val addToQueue = next.filter(updInDegree(_) == 0)

          loop(remainingQueue ++ addToQueue, updInDegree, page :: acc)
    }

    val inDegree = update.pages
      .map(page => page -> relevantOrder.count(_._2.contains(page)))
      .toMap

    loop(Queue.from(inDegree.filter(_._2 == 0).keys), inDegree, Nil)
  }

  private def parseInput(raw: String) = {
    val parts = raw.split("\n\n")

    val orderRules = parts(0).split("\n")
      .map { case s"$x|$y" => OrderingRule(x.toInt, y.toInt) }
      .toList

    val updates = parts(1).split("\n")
      .map(_.split(",").map(_.toInt).toVector)
      .map(Update.apply)
      .toList

    orderRules -> updates
  }

  private val sample =
    """47|53
      |97|13
      |97|61
      |97|47
      |75|29
      |61|13
      |75|53
      |29|13
      |97|29
      |53|29
      |61|53
      |97|53
      |61|29
      |47|13
      |75|47
      |97|75
      |47|61
      |75|61
      |47|29
      |75|13
      |53|13
      |
      |75,47,61,53,29
      |97,61,53,29,13
      |75,29,13
      |75,97,47,61,53
      |61,13,29
      |97,13,75,29,47""".stripMargin

  private val input = Input.asString("day5.txt")

  println(solvePart1.tupled(parseInput(sample))) // 143
  println(solvePart1.tupled(parseInput(input))) // 4924
  println(solvePart2.tupled(parseInput(sample))) // 123
  println(solvePart2.tupled(parseInput(input))) // 6085

}
