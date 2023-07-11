package com.donohoedigital.advent2022

import scala.collection.immutable.HashSet
import scala.collection.mutable

// https://adventofcode.com/2022/day/18
object Dec18 extends App {

  val sample =
    """2,2,2
      |1,2,2
      |3,2,2
      |2,1,2
      |2,3,2
      |2,2,1
      |2,2,3
      |2,2,4
      |2,2,6
      |1,2,5
      |3,2,5
      |2,1,5
      |2,3,5""".stripMargin

  var input = Input.readFile("day18.txt", trim = false).split("\n")

  // Uncomment to run on sample data
  //input = sample.split("\n")

  val boxes = input.zipWithIndex.map{case (line, idx) => {
    val digits = line.split(",").map(_.toInt)
    Box(idx, digits(0), digits(1), digits(2))
  }}

  boxes.foreach { box1 =>
    boxes.foreach { box2 =>
      if (box1.num < box2.num) {
        val shared = box1.sharesSide(box2)
        if (shared.nonEmpty) {
          box1.sharedSides += shared.get
          box2.sharedSides += shared.get
        }
      }
    }
  }

  // Part 1 correct answer is 4474 (sample is 64)
  println("Part1 open sides: "+ boxes.map(_.openSides).sum)

  // Part 2 not attempted
}

case class Side(points: List[P3])

case class Box(num: Int, x: Int, y: Int, z: Int){
  val w = 1
  val h = 1
  val d = 1

  val fbl = P3(x, y, z)
  val fbr = P3(x + w, y, z)
  val ftl = P3(x, y + h, z)
  val ftr = P3(x + w, y + h, z)

  val abl = P3(x, y, z + d)
  val abr = P3(x + w, y, z + d)
  val atl = P3(x, y + h, z + d)
  val atr = P3(x + w, y + h, z + d)

  val front = Side(List(fbl, fbr, ftl, ftr).sorted)
  val rear = Side(List(abl, abr, atl, atr).sorted)
  val top = Side(List(ftl, ftr, atl, atr).sorted)
  val bottom = Side(List(fbl, fbr, abl, abr).sorted)
  val left = Side(List(fbl, ftl, atl, abl).sorted)
  val right = Side(List(fbr, ftr, atr, abr).sorted)

  val sides = HashSet(front, rear, top, bottom, left, right)

  val sharedSides = new mutable.HashSet[Side]()

  def openSides = 6 - sharedSides.size

  def sharesSide(b2: Box): Option[Side] = {
    // brute force it first
    sides.foreach(side =>
      if (b2.sides.contains(side)) {
        return Some(side)
      }
    )
    None
  }
}

case class P3(var x: Int, var y: Int, z: Int) extends Comparable[P3] {
  override def compareTo(o: P3): Int = {
    if (x == o.x) {
      if (y == o.y) {
        z - o.z
      } else y - o.y
    } else x - o.x
  }
}
