package aoc

import aoc.algo.BFS
import aoc.util.Grid

object Day10 extends App {

  private val Start = 0
  private val End = 9

  case class TopographicMap(underlying: Grid[Int]) {
    def starts: IndexedSeq[Grid.Index] = underlying.indices.filter(underlying(_) == Start)
    def adjacent(index: Grid.Index): List[Grid.Index] =
      underlying.adjacent4(index).filter(adj => underlying(adj) == underlying(index) + 1)
    def isEnd(index: Grid.Index): Boolean = underlying(index) == End
  }

  def solvePart1: TopographicMap => Int = solve(getReachableEndCount)

  def solvePart2: TopographicMap => Int = solve(getDistinctTrailCount)

  private def solve(getPathScore: (TopographicMap, Grid.Index) => Int)(map: TopographicMap): Int =
    map.starts.map(getPathScore(map, _)).sum

  private def getReachableEndCount(map: TopographicMap, start: Grid.Index): Int =
    BFS.discoverRegion(start)(map.adjacent).count(map.isEnd)

  private def getDistinctTrailCount(map: TopographicMap, start: Grid.Index): Int = {
    def dfs(index: Grid.Index): Int =
      if (map.isEnd(index)) 1
      else map.adjacent(index).map(dfs).sum

    dfs(start)
  }

  private def parseTopographicMap(s: String) = TopographicMap(Grid.parseCharacterGrid(s).map(_ - '0'))

  private val sample = parseTopographicMap(
    """89010123
      |78121874
      |87430965
      |96549874
      |45678903
      |32019012
      |01329801
      |10456732""".stripMargin
  )

  private val input = parseTopographicMap(Input.asString("day10.txt"))

  println(solvePart1(sample)) // 36
  println(solvePart1(input)) // 587
  println(solvePart2(sample)) // 81
  println(solvePart2(input)) // 1340

}
