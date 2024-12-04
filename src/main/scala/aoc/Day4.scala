package aoc

import aoc.util.Grid
import aoc.util.Grid.Index

import java.time.LocalDate

object Day4 extends App {

  private val Xmas = "XMAS".toIndexedSeq
  private val Mas = "MAS".toIndexedSeq

  type RawInput = String

  def solvePart1(grid: Grid[Char]) = {
    def countXmas(index: Index): Int =
      List(
        grid.rightFrom(index, Xmas.size),
        grid.leftFrom(index, Xmas.size),
        grid.upFrom(index, Xmas.size),
        grid.downFrom(index, Xmas.size),
        grid.diagonalRightDownFrom(index, Xmas.size),
        grid.diagonalRightUpFrom(index, Xmas.size),
        grid.diagonalLeftDownFrom(index, Xmas.size),
        grid.diagonalLeftUpFrom(index, Xmas.size)
      )
        .map(_.map(grid.apply))
        .count(_ == Xmas)

    grid.indices.view
      .filter(grid(_) == Xmas.head)
      .map(countXmas)
      .sum
  }

  def solvePart2(grid: Grid[Char]) = {

    def isXMas(middle: Index): Boolean = {
      List(
        grid.diagonalRightDownFrom(Index(middle.row - 1, middle.column - 1), Mas.size),
        grid.diagonalLeftDownFrom(Index(middle.row - 1, middle.column + 1), Mas.size)
      )
        .map(_.map(grid.apply))
        .forall(diagonal => diagonal == Mas || diagonal == Mas.reverse)

    }

    (for {
      row <- 1 until grid.height - 1
      col <- 1 until grid.width - 1 if grid(row, col) == Mas(1)
    } yield Index(row, col)).count(isXMas)
  }

  private val sample = Grid.parseCharacterGrid(
    """MMMSXXMASM
      |MSAMXMSMSA
      |AMXSXMAAMM
      |MSAMASMSMX
      |XMASAMXAMM
      |XXAMMXXAMA
      |SMSMSASXSS
      |SAXAMASAAA
      |MAMMMXMMMM
      |MXMXAXMASX""".stripMargin
  )

  private val input = Grid.parseCharacterGrid(Input.asString("day4.txt"))

  println(solvePart1(sample)) // 18
  println(solvePart1(input)) // 2591
  println(solvePart2(sample)) // 9
  println(solvePart2(input)) // 1880

}
