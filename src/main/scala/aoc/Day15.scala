package aoc

import aoc.Input as ReadInput
import aoc.algo.BFS
import aoc.util.Direction.{East, North, South, West}
import aoc.util.{Direction, Grid}

import scala.annotation.tailrec

object Day15 extends App {

  case class Input(grid: Grid[Cell], robotMoves: List[Direction])

  enum Cell:
    case Empty, Wall, Box, BoxPartLeft, BoxPartRight, Robot

  object Cell {
    def isBox(cell: Cell): Boolean = cell == Box
    def isBoxPart(cell: Cell): Boolean = cell == Cell.BoxPartLeft || cell == Cell.BoxPartRight
  }

  def solvePart1(input: Input): Int = solve(input, Cell.Box)

  def solvePart2(input: Input): Int = solve(input.copy(grid = expand(input.grid)), Cell.BoxPartLeft)

  private def solve(input: Input, cellToCount: Cell): Int = {

    @tailrec
    def loop(robot: Grid.Index, grid: Grid[Cell], robotMoves: List[Direction]): Grid[Cell] =
      robotMoves match
        case Nil => grid
        case direction :: tail =>
          val nextRobot = robot.adjacent(direction)
          val boxes = getBoxesToMove(grid, nextRobot, direction)
          val mapAfterBoxesMoved = moveBoxes(grid, boxes, direction)
          val canMove = mapAfterBoxesMoved(nextRobot) == Cell.Empty

          loop(if (canMove) nextRobot else robot, mapAfterBoxesMoved, tail)

    val robot = input.grid.findIndex(_ == Cell.Robot).get
    val finalMap = loop(robot, input.grid.updated(robot, Cell.Empty), input.robotMoves)

    finalMap.indices.view
      .filter(finalMap(_) == cellToCount)
      .map(gpsCoordinate)
      .sum
  }

  private def getBoxesToMove(grid: Grid[Cell], startFrom: Grid.Index, direction: Direction) = {

    def otherPartOfBox(boxPart: Grid.Index): Grid.Index =
      if (grid(boxPart) == Cell.BoxPartLeft) boxPart.right
      else boxPart.left

    if (Cell.isBox(grid(startFrom)))
      grid.indicesFromWhile(startFrom, direction, Cell.isBox).toList
    else if (Cell.isBoxPart(grid(startFrom)))
      direction match
        case West | East =>
          grid.indicesFromWhile(startFrom, direction, Cell.isBoxPart).toList

        case North | South =>
          BFS.discoverRegion(startFrom -> false)((boxPart, isFullBox) => {
            val otherPart = Option.unless(isFullBox)(otherPartOfBox(boxPart) -> true).toList
            val adjacent = Option.when(Cell.isBoxPart(grid(boxPart.adjacent(direction))))(
              boxPart.adjacent(direction) -> false)

            otherPart ++ adjacent
          }).map(_._1).distinct
    else Nil
  }

  private def moveBoxes(grid: Grid[Cell], boxes: List[Grid.Index], direction: Direction) = {
    val obstacleExists = boxes.exists(box => grid(box.adjacent(direction)) == Cell.Wall)

    if (obstacleExists) grid
    else {
      val sortField = direction match
        case South => -(_: Grid.Index).row
        case North => (_: Grid.Index).row
        case East => -(_: Grid.Index).column
        case West => (_: Grid.Index).column

      boxes
        .sortBy(sortField)
        .foldLeft(grid) { (acc, box) =>
          acc
            .updated(box, Cell.Empty)
            .updated(box.adjacent(direction), grid(box))
        }
    }
  }

  private def gpsCoordinate(index: Grid.Index) = index.row * 100 + index.column

  private def parseInput(s: String): Input = {
    val parts = s.split("\n\n")

    val map = Grid.parseCharacterGrid(parts(0))
      .map {
        case '.' => Cell.Empty
        case '#' => Cell.Wall
        case '@' => Cell.Robot
        case 'O' => Cell.Box
      }

    val moves = parts(1).replace("\n", "").map(Direction.fromChar).toList

    Input(map, moves)
  }

  private def expand(grid: Grid[Cell]): Grid[Cell] =
    Grid(grid.rows.map(_.flatMap {
      case Cell.Wall => List(Cell.Wall, Cell.Wall)
      case Cell.Box => List(Cell.BoxPartLeft, Cell.BoxPartRight)
      case Cell.Robot => List(Cell.Robot, Cell.Empty)
      case Cell.Empty => List(Cell.Empty, Cell.Empty)
    }))

  private val sample1 = parseInput(
    """########
      |#..O.O.#
      |##@.O..#
      |#...O..#
      |#.#.O..#
      |#...O..#
      |#......#
      |########
      |
      |<^^>>>vv<v>>v<<""".stripMargin
  )

  private val sample2 = parseInput(
    """##########
      |#..O..O.O#
      |#......O.#
      |#.OO..O.O#
      |#..O@..O.#
      |#O#..O...#
      |#O..O..O.#
      |#.OO.O.OO#
      |#....O...#
      |##########
      |
      |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
      |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
      |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
      |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
      |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
      |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
      |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
      |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
      |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
      |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".stripMargin
  )

  private val input = parseInput(ReadInput.asString("day15.txt"))

  println(solvePart1(sample1)) // 2028
  println(solvePart1(sample2)) // 10092
  println(solvePart1(input)) // 1360570
  println(solvePart2(sample2)) // 9021
  println(solvePart2(input)) // 1381446

}
