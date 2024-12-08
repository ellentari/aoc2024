package aoc.util

import aoc.util.Grid.Index

case class Grid[A](rows: IndexedSeq[IndexedSeq[A]]) {

  val topLeftCorner: Index = Index(0, 0)
  val topRightCorner: Index = Index(0, width - 1)
  val bottomLeftCorner: Index = Index(height - 1, 0)
  val bottomRightCorner: Index = Index(height - 1, width - 1)

  def columns: IndexedSeq[IndexedSeq[A]] = (0 until width).map(col => rows.map(_(col)))

  def height: Int = rows.length
  def width: Int = rows.headOption.fold(0)(_.length)

  def apply(index: Index): A = apply(index.row, index.column)
  def apply(row: Int, col: Int): A = rows(row)(col)

  def map[B](f: A => B): Grid[B] = Grid(rows.map(_.map(f)))

  def updated(index: Index, a: A): Grid[A] = updated(index.row, index.column, a)
  def updated(row: Int, col: Int, a: A): Grid[A] = updated(row, col, _ => a)
  def updated(index: Index, f: A => A): Grid[A] = updated(index.row, index.column, f)
  def updated(row: Int, col: Int, f: A => A): Grid[A] =
    Grid(rows.updated(row, rows(row).updated(col, f(apply(row, col)))))

  def upFrom(row: Int): Range = row to 0 by -1
  def upFrom(row: Int, limit: Int): Range = upFrom(row).take(limit)
  def upFrom(index: Index): IndexedSeq[Index] = upFrom(index.row).map(r => Index(r, index.column))
  def upFrom(index: Index, limit: Int): IndexedSeq[Index] = upFrom(index.row, limit).map(r => Index(r, index.column))
  def upFromWhile(index: Index, p: A => Boolean): IndexedSeq[Index] =
    upFrom(index.row).view.map(Index(_, index.column)).takeWhile(i => p(apply(i))).toIndexedSeq

  def downFrom(row: Int): Range = row until height
  def downFrom(row: Int, limit: Int): Range = downFrom(row).take(limit)
  def downFrom(index: Index): IndexedSeq[Index] = downFrom(index.row).map(Index(_, index.column))
  def downFrom(index: Index, limit: Int): IndexedSeq[Index] = downFrom(index.row, limit).map(Index(_, index.column))
  def downFromWhile(index: Index, p: A => Boolean): IndexedSeq[Index] =
    downFrom(index.row).view.map(Index(_, index.column)).takeWhile(i => p(apply(i))).toIndexedSeq

  def leftFrom(col: Int): Range = col to 0 by -1
  def leftFrom(col: Int, limit: Int): Range = leftFrom(col).take(limit)
  def leftFrom(index: Index): IndexedSeq[Index] = leftFrom(index.column).map(Index(index.row, _))
  def leftFrom(index: Index, limit: Int): IndexedSeq[Index] = leftFrom(index.column, limit).map(Index(index.row, _))
  def leftFromWhile(index: Index, p: A => Boolean): IndexedSeq[Index] =
    leftFrom(index.column).view.map(Index(index.row, _)).takeWhile(i => p(apply(i))).toIndexedSeq

  def rightFrom(col: Int): Range = col until width
  def rightFrom(col: Int, limit: Int): Range = rightFrom(col).take(limit)
  def rightFrom(index: Index): IndexedSeq[Index] = rightFrom(index.column).map(Index(index.row, _))
  def rightFrom(index: Index, limit: Int): IndexedSeq[Index] = rightFrom(index.column, limit).map(Index(index.row, _))
  def rightFromWhile(index: Index, p: A => Boolean): IndexedSeq[Index] =
    rightFrom(index.column).view.map(Index(index.row, _)).takeWhile(i => p(apply(i))).toIndexedSeq

  def diagonalRightDownFrom(index: Index): IndexedSeq[Index] =
    downFrom(index.row).zip(rightFrom(index.column)).map(Index.apply.tupled)
  def diagonalRightDownFrom(index: Index, limit: Int): IndexedSeq[Index] =
    downFrom(index.row, limit).zip(rightFrom(index.column, limit)).map(Index.apply.tupled)

  def diagonalRightUpFrom(index: Index): IndexedSeq[Index] =
    upFrom(index.row).zip(rightFrom(index.column)).map(Index.apply.tupled)
  def diagonalRightUpFrom(index: Index, limit: Int): IndexedSeq[Index] =
    upFrom(index.row, limit).zip(rightFrom(index.column, limit)).map(Index.apply.tupled)

  def diagonalLeftDownFrom(index: Index): IndexedSeq[Index] =
    downFrom(index.row).zip(leftFrom(index.column)).map(Index.apply.tupled)
  def diagonalLeftDownFrom(index: Index, limit: Int): IndexedSeq[Index] =
    downFrom(index.row, limit).zip(leftFrom(index.column, limit)).map(Index.apply.tupled)

  def diagonalLeftUpFrom(index: Index): IndexedSeq[Index] =
    upFrom(index.row).zip(leftFrom(index.column)).map(Index.apply.tupled)
  def diagonalLeftUpFrom(index: Index, limit: Int): IndexedSeq[Index] =
    upFrom(index.row, limit).zip(leftFrom(index.column, limit)).map(Index.apply.tupled)

  def indicesFromWhile(index: Index, direction: Direction, p: A => Boolean): IndexedSeq[Index] = direction match
    case Direction.South => upFromWhile(index, p)
    case Direction.North => downFromWhile(index, p)
    case Direction.East => rightFromWhile(index, p)
    case Direction.West => leftFromWhile(index, p)

  def indices: IndexedSeq[Index] = rows.indices.flatMap(row => rows(row).indices.map(Index(row, _)))
  def rowIndices: Range = rows.indices
  def columnIndices: Range = 0 until width

  def isBorder(index: Index): Boolean =
    index.row == 0 || index.row == height - 1 || index.column == 0 || index.column == width - 1

  def bordersIndices: IndexedSeq[Index] =
    (topSideIndices ++ bottomSideIndices ++ rightSideIndices ++ leftSideIndices).distinct
  def topSideIndices: IndexedSeq[Index] = columnIndices.map(Index(0, _))
  def bottomSideIndices: IndexedSeq[Index] = columnIndices.map(Index(height - 1, _))
  def leftSideIndices: IndexedSeq[Index] = rowIndices.map(Index(_, 0))
  def rightSideIndices: IndexedSeq[Index] = rowIndices.map(Index(_, width - 1))

  def adjacent4(index: Index): List[Index] = adjacent4(index.row, index.column)
  def adjacent4(row: Int, col: Int): List[Index] =
    List(
      (row - 1, col),
      (row, col + 1),
      (row + 1, col),
      (row, col - 1)
    )
      .filter(isWithinGrid)
      .map(Index.apply.tupled)

  def adjacent8(index: Index): List[Index] = adjacent8(index.row, index.column)
  def adjacent8(row: Int, col: Int): List[Index] =
    List(
      (row - 1, col),
      (row - 1, col + 1),
      (row, col + 1),
      (row + 1, col + 1),
      (row + 1, col),
      (row + 1, col - 1),
      (row, col - 1),
      (row - 1, col - 1)
    )
      .filter(isWithinGrid)
      .map(Index.apply.tupled)

  def top(index: Index): Option[Index] = {
    val next = index.top
    Option.when(isWithinGrid(next))(next)
  }

  def adjacentTop3(index: Index): List[Index] =
    (-1 to 1).map(d => Index(index.row - 1, index.column + d)).filter(isWithinGrid).toList

  def bottom(index: Index): Option[Index] = {
    val next = index.bottom
    Option.when(isWithinGrid(next))(next)
  }

  def adjacentBottom3(index: Index): List[Index] =
    (-1 to 1).map(d => Index(index.row + 1, index.column + d)).filter(isWithinGrid).toList

  def left(index: Index): Option[Index] = {
    val next = index.left
    Option.when(isWithinGrid(next))(next)
  }

  def adjacentLeft3(index: Index): List[Index] =
    (-1 to 1).map(d => Index(index.row + d, index.column - 1)).filter(isWithinGrid).toList

  def right(index: Index): Option[Index] = {
    val next = index.right
    Option.when(isWithinGrid(next))(next)
  }

  def adjacentRight3(index: Index): List[Index] =
    (-1 to 1).map(d => Index(index.row + d, index.column + 1)).filter(isWithinGrid).toList

  def takeRows(n: Int): Grid[A] =
    if (n == height) this
    else Grid(rows.take(n))

  def takeColumns(n: Int): Grid[A] =
    if (n == width) this
    else Grid(rows.map(_.take(n)))

  def splitAtRow(n: Int): (Grid[A], Grid[A]) = {
    val (top, bottom) = rows.splitAt(n)
    (Grid(top), Grid(bottom))
  }

  def splitAtColumn(n: Int): (Grid[A], Grid[A]) = {
    val (left, right) = rows.map(_.splitAt(n)).unzip
    (Grid(left), Grid(right))
  }

  def sliceVertically(from: Int, until: Int): Grid[A] =
    Grid(rows.map(_.slice(from, until)))

  def sliceHorizontally(from: Int, until: Int): Grid[A] =
    Grid(rows.slice(from, until))

  def flipVertically: Grid[A] = Grid(rows.map(_.reverse))

  def flipHorizontally: Grid[A] = Grid(rows.reverse)

  def indexOf(predicate: A => Boolean): Option[Index] =
    indices.find(index => predicate(apply(index)))

  def transpose: Grid[A] = Grid(rows.transpose)

  def rotateClockwise: Grid[A] = transpose.flipVertically
  def rotateCounterClockwise: Grid[A] = transpose.flipHorizontally

  private def isWithinGrid(cc: (Int, Int)) =
    cc._1 >= 0 && cc._1 < rows.length && cc._2 >= 0 && cc._2 < rows(cc._1).length

  def isWithinGrid(cc: Index): Boolean = isWithinGrid((cc.row, cc.column))

  def format(rowToString: IndexedSeq[A] => String): String =
    rows.map(rowToString).mkString("\n")

  override def toString: String = format(_.mkString(" "))

}

object Grid {

  case class Index(row: Int, column: Int) {

    def top: Index = Index(row - 1, column)
    def bottom: Index = Index(row + 1, column)
    def left: Index = Index(row, column - 1)
    def right: Index = Index(row, column + 1)

    def adjacent(direction: Direction): Grid.Index =
      direction match {
        case Direction.South => top
        case Direction.North => bottom
        case Direction.East => right
        case Direction.West => left
      }
      
    def add(dr: Int, dc: Int): Grid.Index = Grid.Index(row + dr, column + dc)  
    def subtract(dr: Int, dc: Int): Grid.Index = add(-dr, -dc)

  }

  def parseCharacterGrid(raw: String): Grid[Char] =
    Grid(raw.split("\n").map(_.toVector).toVector)

}
