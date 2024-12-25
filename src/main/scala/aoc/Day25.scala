package aoc

import aoc.util.Grid

object Day25 extends App {

  sealed trait Schematic
  object Schematic {

    case class Lock(pinHeights: IndexedSeq[Int], maxHeight: Int) extends Schematic

    case class Key(shapeHeights: IndexedSeq[Int]) extends Schematic {

      def fits(lock: Lock): Boolean =
        lock.pinHeights.zip(shapeHeights).forall(_ + _ <= lock.maxHeight)
    }
  }

  def solvePart1(schematics: List[Schematic]): Int = {
    val locks = schematics.collect { case lock: Schematic.Lock => lock }
    val keys = schematics.collect { case key: Schematic.Key => key }

    locks
      .map(lock => keys.count(_.fits(lock)))
      .sum
  }

  private def parseInput(input: String) = {

    def getHeights(grid: Grid[Char]) =
      grid.columns.map(_.count(_ == '#') - 1)

    input.split("\n\n")
      .map(Grid.parseCharacterGrid)
      .map { grid =>
        if (grid.topSideIndices.forall(grid(_) == '#'))
          Schematic.Lock(getHeights(grid), grid.height - 2)
        else
          Schematic.Key(getHeights(grid))
      }
      .toList
  }

  private val sample = parseInput(
    """#####
      |.####
      |.####
      |.####
      |.#.#.
      |.#...
      |.....
      |
      |#####
      |##.##
      |.#.##
      |...##
      |...#.
      |...#.
      |.....
      |
      |.....
      |#....
      |#....
      |#...#
      |#.#.#
      |#.###
      |#####
      |
      |.....
      |.....
      |#.#..
      |###..
      |###.#
      |###.#
      |#####
      |
      |.....
      |.....
      |.....
      |#....
      |#.#..
      |#.#.#
      |#####""".stripMargin
  )

  private val input = parseInput(Input.asString("day25.txt"))

  println(solvePart1(sample)) // 3
  println(solvePart1(input)) // 2885

}
