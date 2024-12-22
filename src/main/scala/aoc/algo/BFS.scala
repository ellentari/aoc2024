package aoc.algo

import aoc.util.Grid

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable

object BFS {

  def shortestPathLength[A](start: A)(next: A => List[A], isEnd: A => Boolean): Option[Int] =
    shortestPath(start)(next, isEnd).map(_.size - 1)

  def shortestPath[A](start: A)(next: A => List[A], isEnd: A => Boolean): Option[List[A]] = {

    @tailrec
    def loop(queue: Queue[A], parent: Map[A, A], seen: Set[A]): Option[List[A]] =
      queue.dequeueOption match {
        case None => None
        case Some((a, tail)) =>

          if (isEnd(a)) Some(restorePath(a, parent.get))
          else {
            val toVisit = next(a).filterNot(seen.contains)

            loop(tail ++ toVisit, parent ++ toVisit.map(_ -> a), seen ++ toVisit)
          }
      }

    loop(Queue(start), Map.empty, Set(start))
  }

  def connectedComponents[A](grid: Grid[A])(adjacent: Grid.Index => List[Grid.Index]): List[List[Grid.Index]] = {
    val seen = mutable.HashSet.empty[Grid.Index]
    val connectedComponents = mutable.ListBuffer.empty[List[Grid.Index]]
    for (idx <- grid.indices) {
      if (!seen.contains(idx)) {
        val region = discoverRegion(idx)(adjacent)
        connectedComponents.append(region)
        seen.addAll(region)
      }
    }
    connectedComponents.toList
  }

  def discoverRegion[A](start: A)(next: A => List[A]): List[A] = {
    @tailrec
    def loop(queue: Queue[A], seen: Set[A], acc: List[A]): List[A] =
      queue.dequeueOption match {
        case None => acc.reverse
        case Some((a, tail)) =>
          val toVisit = next(a).filterNot(seen.contains)

          loop(tail ++ toVisit, seen ++ toVisit, a :: acc)
      }

    loop(Queue(start), Set(start), Nil)
  }

  def visitAll[A](start: A)(next: A => List[A], onVisit: A => Unit): Unit = {
    @tailrec
    def loop(queue: Queue[A], seen: Set[A]): Unit =
      queue.dequeueOption match {
        case None => ()
        case Some((a, tail)) =>
          onVisit(a)

          val toVisit = next(a).filterNot(seen.contains)

          loop(tail ++ toVisit, seen ++ toVisit)
      }

    loop(Queue(start), Set(start))
  }

  def restorePath[A](a: A, parent: A => Option[A]): List[A] = {
    @tailrec
    def loop(a: A, acc: List[A]): List[A] =
      parent(a) match {
        case None => acc
        case Some(b) => loop(b, b :: acc)
      }

    loop(a, List(a))
  }

}
