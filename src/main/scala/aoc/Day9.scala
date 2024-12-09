package aoc

import scala.annotation.tailrec
import scala.collection.mutable

object Day9 extends App {

  private val FreeSpace = -1

  def solvePart1(input: Vector[Int]): Long = solve(compactWithFragmentation)(input)

  def solvePart2(input: Vector[Int]): Long = solve(compactMovingFullBlocks)(input)

  private def solve(compact: Array[Int] => Unit)(input: Vector[Int]): Long = {
    val unwrapped = unwrap(input)

    compact(unwrapped)

    calculateChecksum(unwrapped)
  }

  private def unwrap(vector: Vector[Int]): Array[Int] = {
    val unwrapped = Array.ofDim[Int](vector.sum)

    var fileId = 0
    var unwrappedIndex = 0

    for (i <- vector.indices) {
      val isFreeSpace = (i % 2) != 0

      val toAppend =
        if (isFreeSpace) FreeSpace
        else {
          val v = fileId
          fileId += 1
          v
        }

      for (_ <- 0 until vector(i)) {
        unwrapped(unwrappedIndex) = toAppend
        unwrappedIndex += 1
      }
    }

    unwrapped
  }

  private def compactWithFragmentation(array: Array[Int]): Unit = {

    var freeSpaceIndex = 0
    while (array(freeSpaceIndex) != FreeSpace)
      freeSpaceIndex += 1

    var end = array.length - 1
    while (array(end) == FreeSpace)
      end -= 1

    while (freeSpaceIndex < end) {

      array(freeSpaceIndex) = array(end)
      array(end) = FreeSpace

      freeSpaceIndex += 1
      end -= 1

      while (freeSpaceIndex < end && array(freeSpaceIndex) != FreeSpace)
        freeSpaceIndex += 1

      while (freeSpaceIndex < end && array(end) == FreeSpace)
        end -= 1
    }
  }

  private def compactMovingFullBlocks(array: Array[Int]): Unit = {
    val freeSpaces = mutable.ArrayBuffer.from(detectFreeSpaces(array))

    for (blockEnd <- array.indices.reverse
         if array(blockEnd) != FreeSpace && (blockEnd == array.length - 1 || array(blockEnd) != array(blockEnd + 1))) {

      var blockStart = blockEnd
      while (blockStart > 0 && array(blockStart - 1) == array(blockEnd))
        blockStart -= 1

      val blockLength = blockEnd - blockStart + 1

      val freeSpaceI = freeSpaces.indices.find(i => freeSpaces(i)._1 < blockStart && blockLength <= freeSpaces(i)._2)

      freeSpaceI.foreach { freeSpaceI =>
        val (freeSpaceStart, freeSpaceLength) = freeSpaces(freeSpaceI)

        move(array, blockStart, freeSpaceStart, blockLength)

        val remainingFreeSpace = freeSpaceLength - blockLength

        if (remainingFreeSpace > 0)
          freeSpaces.update(freeSpaceI, (freeSpaceStart + blockLength, remainingFreeSpace))
        else
          freeSpaces.remove(freeSpaceI)
      }
    }
  }

  private def detectFreeSpaces(array: Array[Int]): List[(Int, Int)] = {

    @tailrec
    def loop(i: Int, currentLength: Int, acc: List[(Int, Int)]): List[(Int, Int)] =
      if (i >= array.length) (if (currentLength > 0) (i - currentLength, currentLength) :: acc else acc).reverse
      else if (array(i) == FreeSpace) loop(i + 1, currentLength + 1, acc)
      else loop(i + 1, 0, if (currentLength > 0) (i - currentLength, currentLength) :: acc else acc)

    loop(0, 0, Nil)
  }

  private def move(array: Array[Int], srcStart: Int, dstStart: Int, length: Int): Unit =
    for (i <- 0 until length) {
      array(dstStart + i) = array(srcStart + i)
      array(srcStart + i) = FreeSpace
    }

  private def calculateChecksum(array: Array[Int]) =
    array.indices.view.filter(i => array(i) != FreeSpace).map(i => i.toLong * array(i)).sum

  private def parseVector(s: String): Vector[Int] = s.map(_ - '0').toVector

  private val sample = parseVector("2333133121414131402")
  private val input = parseVector(Input.asString("day9.txt"))

  println(solvePart1(sample)) // 1928
  println(solvePart1(input)) // 6337921897505
  println(solvePart2(sample)) // 2858
  println(solvePart2(input)) // 6362722604045

}
