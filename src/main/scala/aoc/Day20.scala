package aoc

import aoc.algo.BFS
import aoc.util.Grid

object Day20 extends App {

  enum Cell:
    case Empty, Wall, Start, End

  def solve(grid: Grid[Cell], minSavedDistance: Int, maxCheatDistance: Int): Int = {
    val noCheatsRoute = findRoute(grid).toVector

    noCheatsRoute.indices.view
      .flatMap(i => (i + minSavedDistance until noCheatsRoute.length).map(i -> _))
      .count { (i, j) =>
        val cheatFrom = noCheatsRoute(i)
        val cheatTo = noCheatsRoute(j)

        val cheatDistance = cheatFrom.manhattanDistance(cheatTo)
        val normalDistance = j - i
        val savedDistance = normalDistance - cheatDistance

        cheatDistance <= maxCheatDistance && minSavedDistance <= savedDistance
      }
  }

  private def findRoute(grid: Grid[Cell]): List[Grid.Index] = {
    val start = grid.findIndex(_ == Cell.Start).get

    BFS.shortestPath(start)(
      grid.adjacent4(_).filter(grid(_) != Cell.Wall),
      grid(_) == Cell.End
    ).get
  }

  private def parseInput(input: String): Grid[Cell] =
    Grid.parseCharacterGrid(input).map {
      case '.' => Cell.Empty
      case '#' => Cell.Wall
      case 'S' => Cell.Start
      case 'E' => Cell.End
    }

  private val sample = parseInput(
    """###############
      |#...#...#.....#
      |#.#.#.#.#.###.#
      |#S#...#.#.#...#
      |#######.#.#.###
      |#######.#.#...#
      |#######.#.###.#
      |###..E#...#...#
      |###.#######.###
      |#...###...#...#
      |#.#####.#.###.#
      |#.#...#.#.#...#
      |#.#.#.#.#.#.###
      |#...#...#...###
      |###############""".stripMargin
  )

  private val input = parseInput(Input.asString("day20.txt"))

  println(solve(sample, minSavedDistance = 1, maxCheatDistance = 2)) // 44
  println(solve(input, minSavedDistance = 100, maxCheatDistance = 2)) // 1311
  println(solve(sample, minSavedDistance = 50, maxCheatDistance = 20)) // 285
  println(solve(input, minSavedDistance = 100, maxCheatDistance = 20)) // 961364

}
