package aoc

import aoc.algo.BFS
import aoc.util.Direction.*
import aoc.util.{Direction, Grid}

object Day12 extends App {

  case class Region(area: Int, perimeter: List[(Grid.Index, Direction)]) {

    def perimeterLength: Int = perimeter.size

    def perimeterSideCount: Int = {
      val sameRow = perimeter
        .filter((_, dir) => dir == North || dir == South)
        .groupMap((idx, dir) => (idx.row, dir))(_._1)

      val sameColumn = perimeter
        .filter((_, dir) => dir == East || dir == West)
        .groupMap((idx, dir) => (idx.column, dir))(_._1)

      val rowSides = sameRow
        .map { (_, row) =>
          val sortedRow = row.sortBy(_.column).toVector

          (1 until sortedRow.length)
            .count(i => sortedRow(i).column != sortedRow(i - 1).column + 1) + 1
        }
        .sum

      val columnSides = sameColumn
        .map { (_, column) =>
          val sortedColumn = column.sortBy(_.row).toVector

          (1 until sortedColumn.length)
            .count(i => sortedColumn(i).row != sortedColumn(i - 1).row + 1) + 1
        }
        .sum

      rowSides + columnSides
    }
  }

  def solvePart1(grid: Grid[Char]): Int = {

    def fencingPrice(region: Region): Int =
      region.area * region.perimeterLength

    solve(grid)(fencingPrice)
  }

  def solvePart2(grid: Grid[Char]): Int = {

    def fencingPrice(region: Region): Int =
      region.area * region.perimeterSideCount

    solve(grid)(fencingPrice)
  }

  private def solve(grid: Grid[Char])(getFencingPrice: Region => Int) = {
    val regions = getRegions(grid)

    regions.map(getFencingPrice).sum
  }

  private def getRegions(grid: Grid[Char]) =
    BFS.connectedComponents(grid)(idx => grid.adjacent4(idx).filter(grid(_) == grid(idx)))
      .map { region =>
        val perimeter = region.flatMap { idx =>
          idx.adjacent4WithDirection
            .filter((adj, _) => !grid.isWithinGrid(adj) || grid(adj) != grid(idx))
            .map((_, dir) => idx -> dir)
        }

        Region(region.size, perimeter)
      }

  private val sample1 = Grid.parseCharacterGrid(
    """AAAA
      |BBCD
      |BBCC
      |EEEC""".stripMargin
  )

  private val sample2 = Grid.parseCharacterGrid(
    """OOOOO
      |OXOXO
      |OOOOO
      |OXOXO
      |OOOOO""".stripMargin
  )

  private val sample3 = Grid.parseCharacterGrid(
    """RRRRIICCFF
      |RRRRIICCCF
      |VVRRRCCFFF
      |VVRCCCJFFF
      |VVVVCJJCFE
      |VVIVCCJJEE
      |VVIIICJJEE
      |MIIIIIJJEE
      |MIIISIJEEE
      |MMMISSJEEE""".stripMargin
  )

  private val sample4 = Grid.parseCharacterGrid(
    """EEEEE
      |EXXXX
      |EEEEE
      |EXXXX
      |EEEEE""".stripMargin
  )

  private val sample5 = Grid.parseCharacterGrid(
    """AAAAAA
      |AAABBA
      |AAABBA
      |ABBAAA
      |ABBAAA
      |AAAAAA""".stripMargin
  )

  private val input = Grid.parseCharacterGrid(Input.asString("day12.txt"))

  println(solvePart1(sample1)) // 140
  println(solvePart1(sample2)) // 772
  println(solvePart1(sample3)) // 1930
  println(solvePart1(input)) // 1550156

  println(solvePart2(sample1)) // 80
  println(solvePart2(sample2)) // 436
  println(solvePart2(sample4)) // 236
  println(solvePart2(sample5)) // 368
  println(solvePart2(input)) // 946084

}
