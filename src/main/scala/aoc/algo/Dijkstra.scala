package aoc.algo

import scala.collection.mutable
import scala.math.Numeric.Implicits.infixNumericOps

object Dijkstra {

  def findMinPathCost[A](start: A)(next: A => List[(A, Int)], isEnd: A => Boolean): Option[Int] =
    findMinPath(start)(next, isEnd).map(_._1)

  private def findMinPath[A](start: A)(next: A => List[(A, Int)], isEnd: A => Boolean): Option[(Int, List[A])] = {
    val priorityQueue = mutable.PriorityQueue.empty[(A, Int)](Ordering.by[(A, Int), Int](-_._2))
    val totalCost = mutable.HashMap.empty[A, Int]
    val parent = mutable.HashMap.empty[A, A]
    val processed = mutable.HashSet.empty[A]

    totalCost.update(start, 0)
    priorityQueue.enqueue((start, 0))

    while (priorityQueue.nonEmpty) {
      val (a, _) = priorityQueue.dequeue()

      if (!processed(a)) {

        val costA = totalCost(a)

        if (isEnd(a))
          return Some((costA, BFS.restorePath(a, parent.get)))
        else
          for ((b, costB) <- next(a) if !processed.contains(b) && totalCost.get(b).forall(_ > costA + costB)) {
            val totalBCost = costA + costB

            parent.update(b, a)
            totalCost.update(b, totalBCost)
            priorityQueue.enqueue((b, totalBCost))
          }

        processed.add(a)
      }
    }

    None
  }

  def findAllMinPathsEntries[A](start: A)(next: A => List[(A, Int)], isEnd: A => Boolean): Option[Set[A]] = {
    val priorityQueue = mutable.PriorityQueue.empty[(A, Int)](Ordering.by[(A, Int), Int](-_._2))
    val totalCost = mutable.HashMap.empty[A, Int]
    val parent = mutable.HashMap.empty[A, List[A]]
    val processed = mutable.HashSet.empty[A]

    totalCost.update(start, 0)
    priorityQueue.enqueue((start, 0))

    while (priorityQueue.nonEmpty) {
      val (a, cost) = priorityQueue.dequeue()

      if (!processed(a)) {

        val costA = totalCost(a)

        if (isEnd(a))
          return Some(restoreVisited(a, parent.toMap))
        else

          for ((b, costB) <- next(a) if !processed.contains(b) && totalCost.get(b).forall(_ >= costA + costB)) {
            val totalBCost = costA + costB
            val existingTotalBCost = totalCost.get(b)

            val parents =
              if (existingTotalBCost.contains(totalBCost)) a :: parent.getOrElse(b, Nil)
              else List(a)

            parent.update(b, parents)
            totalCost.update(b, totalBCost)
            priorityQueue.enqueue((b, totalBCost))
          }

        processed.add(a)
      }
    }

    None
  }

  private def restoreVisited[A](a: A, parent: Map[A, List[A]]) = {
    val all = mutable.HashSet.empty[A]

    def loop(a: A): Unit = {
      all.add(a)
      parent.getOrElse(a, Nil).foreach(loop)
    }

    loop(a)

    all.toSet
  }
  
}
