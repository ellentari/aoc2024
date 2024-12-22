package aoc

import aoc.algo.{BFS, BFS_, Dijkstra}
import aoc.automation.InputDownload
import aoc.util.Direction.{East, West}
import aoc.util.{Direction, Grid}

import java.time.LocalDate
import scala.collection.mutable

object Day16 extends App {

  private val ForwardPoints = 1
  private val TurnPoints = 1000

  private val InitialDirection: Direction = East

  enum Cell:
    case Empty, Wall, Start, End

  case class State(position: Grid.Index, direction: Direction) {

    def nextStates(grid: Grid[Cell]): List[(State, Int)] = {
      val forward = grid.adjacent(position, direction)
        .filterNot(grid(_) == Cell.Wall)
        .map(nextPosition => copy(position = nextPosition))
        .map(_ -> ForwardPoints)

      val turns = Direction.turns(direction)
        .map(nextDirection => copy(direction = nextDirection))
        .map(_ -> TurnPoints)

      forward.toList ::: turns
    }

    def isEnd(grid: Grid[Cell]): Boolean =
      grid(position) == Cell.End

  }

  def solvePart1(grid: Grid[Cell]): Int = {
    val start = grid.findIndex(_ == Cell.Start).get
    val initialState = State(start, InitialDirection)

    Dijkstra.findMinPathCost(initialState)(_.nextStates(grid), _.isEnd(grid)).get
  }

  def solvePart2(grid: Grid[Cell]): Int = {
    val start = grid.findIndex(_ == Cell.Start).get
    val initialState = State(start, InitialDirection)

    Dijkstra.findAllMinPathsEntries(initialState)(_.nextStates(grid), _.isEnd(grid)).get
      .map(_.position)
      .size
  }

  private def getNextMoves(grid: Grid[Cell])(position: Grid.Index, direction: Direction) = {
    val forward = grid.adjacent(position, direction)
      .filterNot(grid(_) == Cell.Wall)
      .map(_ -> direction)
      .map(_ -> ForwardPoints)

    val turn = List(
      Direction.turnLeft(direction),
      Direction.turnRight(direction),
    )
      .map(position -> _)
      .map(_ -> TurnPoints)

    forward.toList ::: turn
  }

  private def parseInput(input: String) =
    Grid.parseCharacterGrid(input)
      .map {
        case '.' => Cell.Empty
        case '#' => Cell.Wall
        case 'S' => Cell.Start
        case 'E' => Cell.End
      }

  private val sample1 = parseInput(
    """###############
      |#.......#....E#
      |#.#.###.#.###.#
      |#.....#.#...#.#
      |#.###.#####.#.#
      |#.#.#.......#.#
      |#.#.#####.###.#
      |#...........#.#
      |###.#.#####.#.#
      |#...#.....#.#.#
      |#.#.#.###.#.#.#
      |#.....#...#.#.#
      |#.###.#.#.#.#.#
      |#S..#.....#...#
      |###############""".stripMargin
  )

  private val sample2 = parseInput(
    """#################
      |#...#...#...#..E#
      |#.#.#.#.#.#.#.#.#
      |#.#.#.#...#...#.#
      |#.#.#.#.###.#.#.#
      |#...#.#.#.....#.#
      |#.#.#.#.#.#####.#
      |#.#...#.#.#.....#
      |#.#.#####.#.###.#
      |#.#.#.......#...#
      |#.#.###.#####.###
      |#.#.#...#.....#.#
      |#.#.#.#####.###.#
      |#.#.#.........#.#
      |#.#.#.#########.#
      |#S#.............#
      |#################""".stripMargin
  )

  private val input = parseInput(Input.asString("day16.txt"))

  println(solvePart1(sample1)) // 7036
  println(solvePart1(sample2)) // 11048
  println(solvePart1(input)) // 93436
  println(solvePart2(sample1)) // 45
  println(solvePart2(sample2)) // 64
  println(solvePart2(input)) // 486

}
