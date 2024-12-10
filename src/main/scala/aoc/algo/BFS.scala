package aoc.algo

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object BFS {

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

  private def visitAll[A](start: A)(next: A => List[A], onVisit: A => Unit): Unit = {
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

}
