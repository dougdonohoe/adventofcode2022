package com.donohoedigital.advent2022

import java.util
import scala.collection.mutable

// https://adventofcode.com/2022/day/20
object Dec20 extends App {

  val sample =
    """1
      |2
      |-3
      |3
      |-2
      |0
      |4""".stripMargin

  var input = Input.readFile("day20.txt", trim = false).split("\n")

  // Uncomment to run on sample data
  //input = sample.split("\n")

  val nums = input.map(_.toInt)

  // Part 1 - correct answer is 8764
  val decoder = Decoder(nums)
  decoder.process()
  println("Part 1 SUM of coordinates is " + decoder.coordinates)

  // Part 2 - correct answer is 535648840980
  val start = System.currentTimeMillis()
  val decoder2 = Decoder(nums, 10, 811589153L)
  decoder2.process()
  println("Part 2 SUM of coordinates is " + decoder2.coordinates)
  println("Took : " + (System.currentTimeMillis() - start) / 1000.0 + " seconds")
}

case class Decoder(nums: Array[Int], loops: Int = 1, key: Long = 1L) {

  val coords = nums.zipWithIndex.map { case (num, idx) => Coord(idx, num * key) }
  val len = nums.length
  val zero: Coord = coords.find(_.value == 0).get

  val list = new util.LinkedList[Coord]
  val orig = new mutable.ListBuffer[Coord]()
  coords.foreach { c =>
    list.add(c)
    orig += c
  }

  def process(): Unit = {
    0 until loops foreach { x =>
      loop()
    }
  }

  def loop(): Unit = {
    orig.foreach { c =>
      val startingIndex = list.indexOf(c)
      if (c.value != 0) {

        var newIndex = (startingIndex + c.value) % (len - 1) // mod by 'len - 1' - to account for removal from list
        if (newIndex < 0) newIndex += len - 1
        if (newIndex >= len) newIndex -= len - 1

        // move
        list.remove(startingIndex)
        list.add(newIndex.toInt, c)
      }
    }
  }

  def coordinates(): Long = {
    val idxZero = list.indexOf(zero)
    val grove = List(1000, 2000, 3000) map { n =>
      val n1000 = (idxZero + n) % len
      println("Index of zero is " + idxZero + " + " + n + " is " + n1000 + " = " + list.get(n1000))
      list.get(n1000).value
    }
    grove.sum
  }
}

case class Coord(origIndex: Int, value: Long) {
  override def toString: String = value.toString
}