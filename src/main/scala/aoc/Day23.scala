package aoc

import scala.collection.mutable

object Day23 extends App {

  case class LANEdge(a: String, b: String)

  def solvePart1(edges: List[LANEdge]): Int = {
    val adjList = makeAdjacencyList(edges)

    val allSetsOf3 = (for {
      a <- adjList.keys if a.startsWith("t")
      b <- adjList.getOrElse(a, Nil)
      c <- adjList.getOrElse(b, Nil) if adjList.get(c).exists(_.contains(a))
    } yield List(a, b, c).sorted).toSet

    allSetsOf3.size
  }

  def solvePart2(edges: List[LANEdge]): String = {
    val adjList = makeAdjacencyList(edges)
    val cliques = bronKerbosch(adjList)
    val maxClique = cliques.maxBy(_.size)

    maxClique.toList.sorted.mkString(",")
  }

  private def makeAdjacencyList(edges: List[LANEdge]) =
    edges
      .flatMap { edge =>
        List(
          edge.a -> edge.b,
          edge.b -> edge.a
        )
      }
      .groupMap(_._1)(_._2)

  private def bronKerbosch(adjList: Map[String, List[String]]): List[Set[String]] = {
    var cliques = List.empty[Set[String]]

    def loop(currentClique: Set[String], candidates: Set[String], finished: Set[String]): Unit = {
      if (candidates.isEmpty && finished.isEmpty)
        cliques = currentClique :: cliques
      else {
        var remainingCandidates = candidates
        var currentFinished = finished

        for (candidate <- candidates) {
          loop(
            currentClique + candidate,
            remainingCandidates.intersect(adjList.getOrElse(candidate, Nil).toSet),
            currentFinished.intersect(adjList.getOrElse(candidate, Nil).toSet)
          )

          remainingCandidates -= candidate
          currentFinished += candidate
        }
      }
    }

    loop(Set.empty, adjList.keySet, Set.empty)

    cliques
  }

  private def parseEdge(s: String) = s match
    case s"$a-$b" => LANEdge(a, b)

  private val sample =
    """kh-tc
      |qp-kh
      |de-cg
      |ka-co
      |yn-aq
      |qp-ub
      |cg-tb
      |vc-aq
      |tb-ka
      |wh-tc
      |yn-cg
      |kh-ub
      |ta-co
      |de-co
      |tc-td
      |tb-wq
      |wh-td
      |ta-ka
      |td-qp
      |aq-cg
      |wq-ub
      |ub-vc
      |de-ta
      |wq-aq
      |wq-vc
      |wh-yn
      |ka-de
      |kh-ta
      |co-tc
      |wh-qp
      |tb-vc
      |td-yn""".stripMargin.split("\n").toList.map(parseEdge)

  private val input = Input.asList("day23.txt").map(parseEdge)

  println(solvePart1(sample)) // 7
  println(solvePart1(input)) // 1075
  println(solvePart2(sample)) // co,de,ka,ta
  println(solvePart2(input)) // az,cg,ei,hz,jc,km,kt,mv,sv,sx,wc,wq,xy

}
