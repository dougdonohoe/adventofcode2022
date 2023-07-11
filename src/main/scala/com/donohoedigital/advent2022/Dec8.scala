package com.donohoedigital.advent2022

import scala.collection.mutable.ListBuffer

object Dec8 {

  val sample =
    """30373
25512
65332
33549
35390"""

  def main(args: Array[String]): Unit = {

    val root = Dir("/", null)
    val lines = Input.readFile("day8.txt").split("\n")
    //val lines = sample.split("\n")
    println("Lines: \n" + lines.mkString("\n"))

    val data = Array.ofDim[Int](lines.length, lines(0).length)
    lines.zipWithIndex.foreach { case (line, row) => {
      line.zipWithIndex.foreach { case (height, col) =>
        data(row)(col) = height.toString.toInt
      }
    }
    }
    val map = TreeMap(data)
    //    println("Line 1: " + lines(0))
    //    println("Row 1:  " + data(0).mkString(""))
    //    println("Row 1:  " + map.row(0).mkString(""))
    //    println("Col 1:  " + map.col(0).mkString(""))
    //    map.explore(0, 0)
    //    map.explore(0, 4)
    //
    //    map.explore(1, 0)
    //    map.explore(1, 1)
    //    map.explore(1, 2)
    //    map.explore(1, 3)
    //    map.explore(1, 4)
    //
    //    map.explore(3, 0)
    map.explore(2, 0)
    map.explore(2, 1)
    map.explore(2, 2)
    map.explore(2, 3)
    map.explore(2, 4)

    println("Lines: \n" + lines.mkString("\n"))

    // Part 1 - correct answer is 1533
    println("Part 1 - Total visible: " + map.countVisible())

    // Part 2 - correct answer is 345744
    println("Part 2 - Highest scenic: " + map.highestScenic())

  }

}


case class TreeMap(map: Array[Array[Int]]) {

  private def numRows = map.length

  private def numCols = map(0).length

  // Part 1

  def countVisible(): Int = {
    var total = 0
    for (r <- 0 until numRows) {
      for (c <- 0 until numCols) {
        total += countVisible(r, c)
      }
    }
    total
  }

  def countVisible(r: Int, c: Int): Int = {
    if (visibleDown(r, c) || visibleUp(r, c) || visibleRight(r, c) || visibleLeft(r, c)) 1 else 0
  }

  def visibleUp(r: Int, c: Int): Boolean = {
    if (r == 0) return true
    val col = getCol(c)
    val current = col(r)
    !col.slice(0, r).exists(x => x >= current)
  }

  def visibleDown(r: Int, c: Int): Boolean = {
    if (r == numRows - 1) return true
    val col = getCol(c)
    val current = col(r)
    !col.slice(r + 1, numRows).exists(x => x >= current)
  }

  def visibleLeft(r: Int, c: Int): Boolean = {
    if (c == 0) return true
    val row = getRow(r)
    val current = row(c)
    !row.slice(0, c).exists(x => x >= current)
  }

  def visibleRight(r: Int, c: Int): Boolean = {
    if (c == numCols - 1) return true
    val row = getRow(r)
    val current = row(c)
    !row.slice(c + 1, numCols).exists(x => x >= current)
  }

  // Part 2


  def highestScenic(): Int = {
    var highest = 0
    for (r <- 0 until numRows) {
      for (c <- 0 until numCols) {
        val s = countScenic(r, c)
        if (s > highest) highest = s
      }
    }
    // do it more functional
    val counts = map.zipWithIndex.flatMap { case (col: Array[Int], r: Int) =>
      col.zipWithIndex.map { case (_: Int, c: Int) => {
        countScenic(r, c)
      }
      }
    }
    counts.max
    assert(counts.max == highest)
    highest
  }

  def countScenic(r: Int, c: Int): Int = {
    scenicDown(r, c) * scenicUp(r, c) * scenicRight(r, c) * scenicLeft(r, c)
  }

  def scenicUp(r: Int, c: Int): Int = {
    if (r == 0) return 0
    val col = getCol(c)
    val current = col(r)
    var higher = col.slice(0, r).lastIndexWhere(_ >= current)
    if (higher == -1) {
      higher = 0
    }
    val score = col.slice(higher, r).length
    score
  }

  def scenicDown(r: Int, c: Int): Int = {
    if (r == numRows - 1) return 0
    val col = getCol(c)
    val current = col(r)
    var higher = col.slice(r + 1, numRows).indexWhere(_ >= current)
    if (higher == -1) {
      higher = numRows
    }
    val score = col.slice(r + 1, r + 1 + higher + 1).length
    score
  }

  def scenicLeft(r: Int, c: Int): Int = {
    if (c == 0) return 0
    val row = getRow(r)
    val current = row(c)
    var higher = row.slice(0, c).lastIndexWhere(_ >= current)
    if (higher == -1) {
      higher = 0
    }
    val score = row.slice(higher, c).length
    score
  }

  def scenicRight(r: Int, c: Int): Int = {
    if (c == numCols - 1) return 0
    val row = getRow(r)
    val current = row(c)
    var higher = row.slice(c + 1, numCols).indexWhere(_ >= current)
    if (higher == -1) {
      higher = numCols
    }
    val score = row.slice(c + 1, c + 1 + higher + 1).length
    score
  }


  // Helpers

  def getRow(r: Int): Array[Int] = {
    map(r)
  }

  def getCol(c: Int): Array[Int] = {
    map.map(row => row(c))
  }

  // Explore

  def explore(r: Int, c: Int): Unit = {
    //println("Up from   " + r + "," + c + ": " + visibleUp(r, c))
    //println("Down from " + r + "," + c + ": " + visibleDown(r, c))
    //println("Left from  " + r + "," + c + ": " + visibleLeft(r, c))
    //println("Right from " + r + "," + c + ": " + visibleRight(r, c))

    //println("Scenic Up from   " + r + "," + c + ": " + scenicUp(r, c))
    //println("Scenic Down from   " + r + "," + c + ": " + scenicDown(r, c))
  }

}
