package com.donohoedigital.advent2022

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

// https://adventofcode.com/2022/day/13
object Dec13 extends App {

  val part2Extra =
    """
      |[[2]]
      |[[6]]
      |""".stripMargin

  val lines = Input.readFile("day13.txt", trim = false).split("\n")
  val linesPart2 = (Input.readFile("day13.txt", trim = false) + part2Extra).split("\n")

  // Part 1 - correct answer is 5390
  val pairs = lines.filter(_.nonEmpty).grouped(2).zipWithIndex.map(t => {
    MessagePair(t._2+1, Entry(t._1(0)), Entry(t._1(1)))
  }).toList
  val rightOrder = pairs.filter(_.rightOrder())
  println("Part 1 sum of correct ordered indicies: " + rightOrder.map(_.index).sum)

  // Part 2 - correct answer is 19261
  val msgs = linesPart2.filter(_.nonEmpty).map(Entry(_)).toList
  val sorted = msgs.sortWith{ case (a, b) => a.isBefore(b).get}
  println("Part 2 product of special keys: " +
    (sorted.indexWhere(_.toString == "[[2]]") + 1) *
    (sorted.indexWhere(_.toString == "[[6]]") + 1)
  )
}

// A pair of messages
case class MessagePair(index: Int, left: Entry, right: Entry)
{
  def rightOrder(): Boolean = {
    left.isBefore(right).get
  }
}

// An entry in a message.  It's turtles all the way down
case class Entry(list: ListBuffer[Entry], num: Option[Int], verbose: Boolean = false) {

  def isBefore(other: Entry, indent: String = ""): Option[Boolean] = {
    if (verbose) println(indent + "- Compare " + this + " vs " + other)

    // both are numbers
    if (num.nonEmpty && other.num.nonEmpty) {
      if (num.get < other.num.get) {
        // yes smaller, stop processing list
        Some(true)
      } else if (num.get == other.num.get) {
        // None means equal, keep processing list
        None
      } else {
        // left side is larger, so not equal
        Some(false)
      }
      // either one or the other is a list - force the non lists into lists
    } else {
      val left = mutable.Stack.from((if (num.nonEmpty) Entry(ListBuffer.fill(1)(this), None) else this).list)
      val right = mutable.Stack.from((if (other.num.nonEmpty) Entry(ListBuffer.fill(1)(other), None) else other).list)

      while (left.nonEmpty) {
        if (right.isEmpty) {
          // right ran out of entries, so we are done
          return Some(false)
        }
        val l = left.pop()
        val r = right.pop()

        val compare = l.isBefore(r, indent + "  ")
        if (compare.nonEmpty) {
          return compare
        } else {
          // keep going
        }
      }
      if (right.isEmpty) {
        // none means equal, keep processing list
        None
      } else {
        // left side is smaller, order is correct
        Some(true)
      }
    }
  }

  // print data like in problem description
  override def toString: String = {
    val sb = new mutable.StringBuilder()
    if (list == null) {
      sb.append(num.get)
    } else {
      sb.append("[")
      sb.append(list.map(_.toString).mkString(","))
      sb.append("]")
    }
    sb.toString()
  }
}

object Entry {

  // parse data
  def apply(line: String): Entry = {
    var current: Entry = null
    val stack = new mutable.Stack[Entry]()
    var i: Int = 0
    while (i < line.length) {
      val c = line(i)
      c match {
        case '[' =>
          val nu = Entry(new ListBuffer[Entry], None)
          if (current != null) {
            current.list += nu
          }
          current = nu
          stack.push(current)

        case ']' =>
          stack.pop()
          if (stack.nonEmpty) {
            current = stack.head
          }

        case ',' =>

        case _ =>
          if (c == '1') {
            // see if next one is a zero
            if (line(i + 1) == '0') {
              current.list += Entry(null, Some(10))
              i += 1
            } else {
              current.list += Entry(null, Some(1))
            }
          } else {
            current.list += Entry(null, Some(c.toString.toInt))
          }
      }
      i += 1
    }
    current
  }
}