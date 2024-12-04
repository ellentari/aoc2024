package aoc.util

enum Direction:
  case South extends Direction
  case North extends Direction
  case East extends Direction
  case West extends Direction

object Direction:
  def opposite(direction: Direction): Direction = direction match
    case South => North
    case North => South
    case East => West
    case West => East

  def turnLeft(direction: Direction): Direction = direction match
    case South => East
    case North => West
    case East => North
    case West => South

  def turnRight(direction: Direction): Direction = direction match
    case South => West
    case North => East
    case East => South
    case West => North
