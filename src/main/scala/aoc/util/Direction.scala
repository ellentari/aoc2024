package aoc.util

enum Direction:
  case South extends Direction
  case North extends Direction
  case East extends Direction
  case West extends Direction

object Direction:

  def fromChar(char: Char): Direction = char match
    case 'v' => South
    case '>' => East
    case '^' => North
    case '<' => West
    case _ => throw new IllegalArgumentException(s"Wrong move: $char")

  def opposite(direction: Direction): Direction = direction match
    case South => North
    case North => South
    case East => West
    case West => East

  def turnRight(direction: Direction): Direction = direction match
    case North => East
    case South => West
    case East => South
    case West => North

  def turnLeft(direction: Direction): Direction = direction match
    case North => West
    case South => East
    case East => North
    case West => South
    
  def turns(direction: Direction): List[Direction] = List(
    turnRight(direction),
    turnLeft(direction)
  )
