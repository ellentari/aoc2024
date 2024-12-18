package aoc

import aoc.algo.BFS
import aoc.util.Grid

import scala.annotation.tailrec

object Day18 extends App {

  enum Cell:
    case Empty, Obstacle

  def solvePart1(bytePositions: List[Grid.Index], size: Int, limit: Int): Int = {
    val grid = buildGrid(size, bytePositions.take(limit))

    findShortestPath(grid).map(_.size - 1).get
  }

  def solvePart2(bytePositions: List[Grid.Index], size: Int, limit: Int): String = {

    @tailrec
    def loop(grid: Grid[Cell], bytePositions: List[Grid.Index], currentPath: Set[Grid.Index]): Option[Grid.Index] = {
      bytePositions match
        case Nil => None
        case bytePosition :: remainingBytePositions =>
          val updGrid = grid.updated(bytePosition, Cell.Obstacle)

          if (currentPath.contains(bytePosition))
            findShortestPath(updGrid) match
              case None =>
                Some(bytePosition)
              case Some(newPath) =>
                loop(updGrid, remainingBytePositions, newPath.toSet)
          else
            loop(updGrid, remainingBytePositions, currentPath)
    }

    val grid = buildGrid(size, bytePositions.take(limit))
    val path = findShortestPath(grid).get

    loop(grid, bytePositions.drop(limit), path.toSet)
      .map(formatAnswer)
      .get
  }

  private def buildGrid(size: Int, obstaclePositions: List[Grid.Index]) =
    obstaclePositions.foldLeft(Grid.fill(size, size)(Cell.Empty))(_.updated(_, Cell.Obstacle))

  private def findShortestPath(grid: Grid[Cell]) =
    BFS.shortestPath(grid.topLeftCorner)(
      grid.adjacent4(_).filter(grid(_) != Cell.Obstacle),
      _ == grid.bottomRightCorner)

  private def parseInput(s: String) =
    s.split("\n")
      .map {
        case s"$x,$y" => Grid.Index(y.toInt, x.toInt)
      }
      .toList

  private def formatAnswer(index: Grid.Index) = s"${index.column},${index.row}"

  private val sample = parseInput(
    """5,4
      |4,2
      |4,5
      |3,0
      |2,1
      |6,3
      |2,4
      |1,5
      |0,6
      |3,3
      |2,6
      |5,1
      |1,2
      |5,5
      |2,5
      |6,5
      |1,4
      |0,4
      |6,4
      |1,1
      |6,1
      |1,0
      |0,5
      |1,6
      |2,0""".stripMargin
  )

  private val input = parseInput(Input.asString("day18.txt"))

  println(solvePart1(sample, 7, 12)) // 22
  println(solvePart1(input, 71, 1024)) // 336
  println(solvePart2(sample, 7, 12)) // 6,1
  println(solvePart2(input, 71, 1024)) // 24,30

}
