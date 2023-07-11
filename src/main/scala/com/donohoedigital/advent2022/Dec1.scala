package com.donohoedigital.advent2022

import scala.collection.mutable

object Dec1 {
  def main(args: Array[String]): Unit = {
    val lines = Input.readFile("day1.txt").split("\n")

    val byNum = lines.map(x => if (x == "") "-1" else x).map(_.toInt)
    val elfs = new mutable.HashMap[Int, Int]()
    var elfNum = 0
    var current = 0
    byNum.foreach(num => {
      if (num == -1) {
        elfNum += 1
        elfs(elfNum) = current
        current = 0
      } else {
        current += num
      }
    })
    val sorted = elfs.toSeq.sortBy(_._2).reverse
    println("Part 1 - Max: " + sorted.take(1).map(_._2).head)
    println("Top 3: " + sorted.take(3).map(_._2).mkString(" "))
    println("Part 2 - SUM of top 3: " + sorted.take(3).map(_._2).sum)
  }
}
