package aoc

import aoc.Day6.Cell.{Empty, Guard, Obstacle}
import aoc.util.Grid.Index
import aoc.util.{Direction, Grid}

import scala.annotation.tailrec

object Day6 extends App {

  sealed trait Cell
  object Cell {
    case object Empty extends Cell
    case object Obstacle extends Cell
    case class Guard(direction: Direction) extends Cell
  }

  case class ObstacleMap(byRow: Map[Int, IndexedSeq[Grid.Index]], byColumn: Map[Int, IndexedSeq[Grid.Index]]) {
    def updated(index: Grid.Index): ObstacleMap =
      ObstacleMap(
        byRow.updatedWith(index.row)(_.map(_ :+ index)),
        byColumn.updatedWith(index.column)(_.map(_ :+ index)),
      )

    def findObstacleTo(index: Grid.Index, direction: Direction): Option[Grid.Index] = direction match
      case Direction.North => byColumn.get(index.column).flatMap(_.view.filter(_.row < index.row).maxByOption(_.row))
      case Direction.South => byColumn.get(index.column).flatMap(_.view.filter(_.row > index.row).minByOption(_.row))
      case Direction.East => byRow.get(index.row).flatMap(_.view.filter(_.column > index.column).minByOption(_.column))
      case Direction.West => byRow.get(index.row).flatMap(_.view.filter(_.column < index.column).maxByOption(_.column))
  }

  object ObstacleMap {
    def fromGrid(grid: Grid[Cell]): ObstacleMap = {
      val obstacles = grid.indices.filter(grid(_) == Obstacle)
      val obstaclesByRow = obstacles.groupBy(_.row)
      val obstaclesByCol = obstacles.groupBy(_.column)

      ObstacleMap(obstaclesByRow, obstaclesByCol)
    }
  }

  def solvePart1(grid: Grid[Cell]): Int = {
    val (guardPosition, guard) = findGuard(grid).get
    val gridWithoutGuard = grid.updated(guardPosition, Empty)

    val visitedCells = getGuardRoute(guardPosition, guard.direction, gridWithoutGuard)

    visitedCells.size
  }

  def solvePart2(grid: Grid[Cell]): Int = {
    val (guardPosition, guard) = findGuard(grid).get
    val gridWithoutGuard = grid.updated(guardPosition, Empty)

    val visitedCells = getGuardRoute(guardPosition, guard.direction, gridWithoutGuard)
    val cellsToPlaceObstaclesTo = visitedCells - guardPosition

    val obstacleMap = ObstacleMap.fromGrid(grid)

    cellsToPlaceObstaclesTo
      .count { obstacleIndex =>
        val updObstacleMap = obstacleMap.updated(obstacleIndex)

        cycleExists(updObstacleMap, guardPosition, guard.direction)
      }
  }

  private def findGuard(grid: Grid[Cell]) =
    grid.indices.map(i => i -> grid(i)).collectFirst {
      case (i, g: Guard) => (i, g)
    }

  private def getGuardRoute(guardPosition: Grid.Index, guardDirection: Direction, grid: Grid[Cell]): Set[Grid.Index] = {
    @tailrec
    def loop(position: Grid.Index, direction: Direction, visitedCells: Set[Grid.Index]): Set[Grid.Index] = {
      val path = grid.indicesFromWhile(position, direction, _ != Obstacle)

      val nextPosition = path.last
      val nextDirection = Direction.turnRight(direction)
      val nextVisitedCells = visitedCells ++ path

      if (grid.isBorder(nextPosition)) nextVisitedCells
      else loop(nextPosition, nextDirection, nextVisitedCells)
    }

    loop(guardPosition, guardDirection, Set.empty)
  }

  private def cycleExists(obstacleMap: ObstacleMap, guardPosition: Grid.Index, guardDirection: Direction): Boolean = {

    @tailrec
    def loop(position: Grid.Index, direction: Direction, visited: Set[(Grid.Index, Direction)]): Boolean = {
      obstacleMap.findObstacleTo(position, direction) match
        case None => false
        case Some(obstacle) =>
          val nextPosition = obstacle.adjacent(Direction.opposite(direction))
          val nextDirection = Direction.turnRight(direction)

          if (visited.contains((nextPosition, nextDirection))) true
          else loop(nextPosition, nextDirection, visited + (nextPosition -> nextDirection))
    }

    loop(guardPosition, guardDirection, Set(guardPosition -> guardDirection))
  }

  private def parseGrid(s: String) =
    Grid.parseCharacterGrid(s)
      .map {
        case '.' => Empty
        case '#' => Obstacle
        case guardDirection => Guard(Direction.fromChar(guardDirection))
      }

  private val sample = parseGrid(
    """....#.....
      |.........#
      |..........
      |..#.......
      |.......#..
      |..........
      |.#..^.....
      |........#.
      |#.........
      |......#...""".stripMargin
  )

  private val input = parseGrid(Input.asString("day6.txt"))

  println(solvePart1(sample)) // 41
  println(solvePart1(input)) // 4890
  println(solvePart2(sample)) // 6
  println(solvePart2(input)) // 1995

}
