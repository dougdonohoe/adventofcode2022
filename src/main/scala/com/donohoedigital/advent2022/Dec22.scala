package com.donohoedigital.advent2022

import scala.collection.mutable
import Const22._
import scala.collection.mutable.ListBuffer

// https://adventofcode.com/2022/day/22
object Dec22 extends App {

  val sample =
    """        ...#
      |        .#..
      |        #...
      |        ....
      |...#.......#
      |........#...
      |..#....#....
      |..........#.
      |        ...#....
      |        .....#..
      |        .#......
      |        ......#.
      |
      |10R5L5R10L4R5L5 """.stripMargin

  val sampleCube="""        1111
                   |        1111
                   |        1111
                   |        1111
                   |222233334444
                   |222233334444
                   |222233334444
                   |222233334444
                   |        55556666
                   |        55556666
                   |        55556666
                   |        55556666""".stripMargin

  var input = Input.readFile("day22.txt", trim = false).split("\n")

  // uncomment to use sample data
  //input = sample.split("\n")

  val instructions = input.last.trim
  val map = input.take(input.length - 1).filter(_.nonEmpty)
  val longestMapLine = map.map(_.length).max
  val padded = map.map(line => line.padTo(longestMapLine, " ").mkString(""))
  val asChar = padded.map(_.toCharArray)
  val byCol = asChar.transpose
  val rows = padded.zipWithIndex.map { case (line, y) =>
    MapRowCol(y + 1, line)
  }
  val cols = byCol.zipWithIndex.map { case (col, x) =>
    MapRowCol(x + 1, col.mkString(""))
  }
  val pass = MapPassword(parseInstructions(instructions), rows, cols)

  println(pass.instructions)
  //println(rows.mkString("\n"))
  pass.printGrid()

  // Part 1 - correct answer is 30552
  pass.moveAll()
  println("Part 1 password is " + pass.password)

  // Part 2 - not yet attempted (used python solution)
}

case class MapPassword(instructions: List[Move], rows: Array[MapRowCol], cols: Array[MapRowCol]) {
  var current = Point22(0, rows(0).smallest, Right)

  def row: MapRowCol = rows(current.row)

  def col: MapRowCol = cols(current.col)

  // Facing is 0 for right (>), 1 for down (v), 2 for left (<), and 3 for up (^).
  // The final password is the sum of 1000 times the row, 4 times the column, and the facing.
  def password: Int = {
    val facing = current.direction match {
      case Right => 0
      case Down => 1
      case Left => 2
      case Up => 3
    }
    (1000 * (current.row + 1)) + (4 * (current.col + 1)) + facing
  }

  def moveAll(): Unit = {
    instructions.foreach(doMove)
  }

  def doMove(move: Move) = {
    //println(move)
    move match {
      case TurnLeft() => current.turnLeft()
      case TurnRight() => current.turnRight()
      case Forward(n) =>
        0 until n foreach { _ =>
          moveOne()
          //printGrid()
        }
    }
  }

  // attempt to move one in current direction
  def moveOne(): Unit = {
    current.direction match {
      case Right =>
        row.itemAt(current.col + 1) match {
          case Some(Open) => current.col += 1
          case Some(Wall) =>
          case None =>
            row.itemAt(row.smallest) match {
              case Some(Open) => current.col = row.smallest
              case Some(Wall) =>
              case x => throw new RuntimeException("BAD Right from " + current)
            }
        }

      case Left =>
        row.itemAt(current.col - 1) match {
          case Some(Open) => current.col -= 1
          case Some(Wall) =>
          case None =>
            row.itemAt(row.largest) match {
              case Some(Open) => current.col = row.largest
              case Some(Wall) =>
              case x => throw new RuntimeException("BAD Left from " + current)
            }
        }

      case Up =>
        col.itemAt(current.row - 1) match {
          case Some(Open) => current.row -= 1
          case Some(Wall) =>
          case None =>
            col.itemAt(col.largest) match {
              case Some(Open) => current.row = col.largest
              case Some(Wall) =>
              case x => throw new RuntimeException("BAD Up from " + current)
            }
        }

      case Down =>
        col.itemAt(current.row + 1) match {
          case Some(Open) => current.row += 1
          case Some(Wall) =>
          case None =>
            col.itemAt(col.smallest) match {
              case Some(Open) => current.row = col.smallest
              case Some(Wall) =>
              case x => throw new RuntimeException("BAD Down from " + current)
            }
        }
    }
  }

  def printGrid(): Unit = {
    println("===========================================")
    rows.zipWithIndex.foreach{ case (row, r) =>
      row.data.toCharArray.zipWithIndex.foreach{ case (value, c) =>
        if (r == current.row && c == current.col) {
          print(current.direction)
        } else {
          print(value)
        }
      }
      println()
    }
  }

}

case class Point22(var row: Int, var col: Int, var direction: String) {
  def turnLeft() = {
    direction match {
      case Right => direction = Up
      case Up => direction = Left
      case Left => direction = Down
      case Down => direction = Right
    }
  }

  def turnRight() = {
    direction match {
      case Right => direction = Down
      case Up => direction = Right
      case Left => direction = Up
      case Down => direction = Left
    }
  }
}

case class MapRowCol(num: Int, data: String) {
  val smallest = data.indexWhere(c => c == Open || c == Wall)
  val largest = data.lastIndexWhere(c => c == Open || c == Wall)

  def itemAt(c: Int): Option[Char] = {
    if (c < smallest) None
    else if (c > largest) None
    else Some(data(c))
  }

  override def toString: String = {
    data + " smallest: " + smallest + " largest: " + largest
  }
}

object Const22 {
  val Open = '.'
  val Wall = '#'

  val Left = "<"
  val Right = ">"
  val Down = "v"
  val Up = "^"

  val turnLeft = 'L'
  val turnRight = 'R'

  sealed trait Move

  case class TurnLeft() extends Move

  case class TurnRight() extends Move

  case class Forward(amount: Int) extends Move

  def parseInstructions(line: String): List[Move] = {
    val list = new ListBuffer[Move]
    var current = line
    while (current.nonEmpty) {
      val idx = current.indexWhere(c => c == turnLeft || c == turnRight)
      if (idx == -1) {
        list += Forward(current.toInt)
        current = ""
      } else {
        list += Forward(current.substring(0, idx).toInt)
        val turn = if (current.charAt(idx) == turnLeft) TurnLeft() else TurnRight()
        list += turn
        current = current.substring(idx + 1)
      }
    }
    list.toList
  }
}
