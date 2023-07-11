package com.donohoedigital.advent2022

import scala.collection.mutable

case class Move(num: Int, from: Int, to: Int)

object Dec5 {
  def main(args: Array[String]): Unit = {

    val lines = Input.readFile("day5.txt", trim = false).split("\n")

    // build stacks
    val stacksPart1 = new mutable.ListBuffer[mutable.Stack[String]]
    for (_ <- 1 to 9) {
      stacksPart1 += new mutable.Stack[String]()
    }
    val crates = lines.filter(_.contains("["))
    println(crates.mkString("\n"))
    val stripped = crates.reverse.map(_.grouped(4)).map(
      _.toList.map(
        _.trim.replace("[", "").replace("]", "")))
    stripped.foreach(_.zipWithIndex.foreach { case (elem, idx) =>
      if (elem.nonEmpty) stacksPart1(idx).push(elem)
    })
    println(stacksPart1.mkString("\n"))
    val stacksPart2 = stacksPart1.map(_.clone)

    // build moves
    // move 16 from 2 to 9
    val moves = lines.filter(_.startsWith("move")).map(line => {
      val nums = line.replace("move ", "").replace(
        " from ", ",").replace(
        " to ", ",").split(",")
      Move(nums(0).toInt, nums(1).toInt, nums(2).toInt)
    })
    //println(moves.mkString("\n"))

    // do moves part 1
    moves.foreach(move => {
      Range.inclusive(1, move.num).foreach { x=>
        stacksPart1(move.to-1).push(stacksPart1(move.from-1).pop)
      }
    }
    )
    //println(stacksPart1.mkString("\n"))

    // do moves part 2
    moves.foreach(move => {
      val popped = stacksPart2(move.from-1).take(move.num)
      Range.inclusive(1, move.num).foreach { x=>
        stacksPart2(move.from-1).pop
      }
      stacksPart2(move.to-1).pushAll(popped.reverse)
    }
    )
    //println(stacksPart2.mkString("\n"))

    println("Part 1 answer: " + stacksPart1.map(_.top).mkString(""))
    println("Part 2 answer: " + stacksPart2.map(_.top).mkString(""))

    // Correct:
    // Part 1 answer: DHBJQJCCW
    // Part 2 answer: WJVRLSJJT
  }
}
