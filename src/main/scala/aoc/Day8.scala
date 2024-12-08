package aoc

import aoc.util.Grid

import scala.annotation.tailrec

object Day8 extends App {

  def solvePart1(grid: Grid[Char]): Int = {

    def getAntinodes(antenna1: Grid.Index, antenna2: Grid.Index): List[Grid.Index] =
      List(
        getAntinode(antenna1, antenna2, distance = 1),
        getAntinode(antenna2, antenna1, distance = 1)
      ).filter(grid.isWithinGrid)

    solve(grid, getAntinodes)
  }

  def solvePart2(grid: Grid[Char]): Int = {

    def getAntinodes(antenna1: Grid.Index, antenna2: Grid.Index): List[Grid.Index] = {

      @tailrec
      def loop(from: Grid.Index, to: Grid.Index, distance: Int, acc: List[Grid.Index]): List[Grid.Index] = {
        val antinode = getAntinode(from, to, distance)

        if (grid.isWithinGrid(antinode)) loop(from, to, distance + 1, antinode :: acc)
        else acc
      }

      loop(antenna1, antenna2, 0, Nil) ::: loop(antenna2, antenna1, 0, Nil)
    }

    solve(grid, getAntinodes)
  }

  private def solve(grid: Grid[Char], getAntinodes: (Grid.Index, Grid.Index) => List[Grid.Index]) = {
    val antennas = grid.indices.filter(grid(_) != '.')

    (for {
      i <- antennas.indices
      j <- i + 1 until antennas.length if grid(antennas(i)) == grid(antennas(j))
      antinode <- getAntinodes(antennas(i), antennas(j))
    } yield antinode).distinct.size
  }

  private def getAntinode(from: Grid.Index, to: Grid.Index, distance: Int): Grid.Index = {
    val dr = from.row - to.row
    val dc = from.column - to.column

    to.subtract(dr * distance, dc * distance)
  }

  private val sample = Grid.parseCharacterGrid(
    """............
      |........0...
      |.....0......
      |.......0....
      |....0.......
      |......A.....
      |............
      |............
      |........A...
      |.........A..
      |............
      |............""".stripMargin
  )

  private val input = Grid.parseCharacterGrid(Input.asString("day8.txt"))

  println(solvePart1(sample)) // 14
  println(solvePart1(input)) // 293
  println(solvePart2(sample)) // 34
  println(solvePart2(input)) // 934

}
