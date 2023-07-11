package com.donohoedigital.advent2022

import scala.collection.mutable.ListBuffer

object Dec11 extends App {
  var lines = Input.readFile("day11.txt", trim = false).split("\n")

  // Parse twice since we mutate the data
  val monkeys1 = lines.map(_.trim).grouped(7).map(s => Monkey(s)).toList
  val monkeys2 = lines.map(_.trim).grouped(7).map(s => Monkey(s)).toList

  // Part 1: correct answer is 62491
  println("Monkeys: \n" + monkeys1.mkString("\n") + "\n")
  val game1 = Game(monkeys1.toArray, 3)
  game1.process(20)
  val top2 = game1.monkeys.sortBy(_.inspected).reverse.take(2).map(_.inspected).product
  println("Part 1 - product of top 2 (aka 'monkey business'): " + top2)
  println

  // Part 2: correct answer is 17408399184
  //println("Monkeys 2: \n" + monkeys2.mkString("\n"))
  val game2 = Game(monkeys2.toArray, 1)
  game2.process(10000)
  val top2_game2 = game2.monkeys.sortBy(_.inspected).reverse.take(2).map(_.inspected).product
  println("Part 2 - product of top 2 (aka 'monkey business'): " + top2_game2)
}

case class Game(monkeys: Array[Monkey], worryDivider: Long) {

  // to prevent overflow due to squaring of some numbers ("old * old"), we take the product module the
  // product of all monkey's divisors (which are all prime).  This preserves the divisibility each
  // monkey expects w/out numbers getting to large
  val allDivisors: Long = {
    monkeys.map(_.divisibleBy).product
  }

  def process(rounds: Int): Unit = {
    (0 until rounds).foreach(processRound)
  }

  def processRound(num: Int): Unit = {
    monkeys.foreach(processRound)
  }

  def processRound(monkey: Monkey): Unit = {
    monkey.processAll(monkeys, worryDivider, allDivisors)
  }
}

/**
 * Monkey 0:
 * Starting items: 75, 63
 * Operation: new = old * 3
 * Test: divisible by 11
 *   If true: throw to monkey 7
 *   If false: throw to monkey 2
 */
case class Monkey(num: Int, items: ListBuffer[Long], op: Operation, divisibleBy: Long, trueTo: Int, falseTo: Int) {
  var inspected = 0L

  def processAll(monkeys: Array[Monkey], worryDivider: Long, allDivisors: Long): Unit = {
    while (items.nonEmpty) {
      inspected += 1
      val old = items.remove(0)
      val newLevel = (op.compute(old) % allDivisors) / worryDivider
      val sendTo = if (newLevel % divisibleBy == 0) {
        trueTo
      } else {
        falseTo
      }
      monkeys(sendTo).items.addOne(newLevel)
    }
  }
}

object Monkey {
  def apply(lines: Array[String]): Monkey = {
    val num = lines(0).replace("Monkey ", "").replace(":", "").toInt
    val items = lines(1).replace("Starting items: ", "").split(", ").map(_.toLong)
    val itemsListBuffer = ListBuffer(items: _ *)
    val pattern = "Operation: new = old (.) (.*)".r
    val pattern(op, operand) = lines(2)
    val operation = Operation(op, operand)
    val divisibleBy = lines(3).replace("Test: divisible by ", "").toInt
    val trueTo = lines(4).replace("If true: throw to monkey ", "").toInt
    val falseTo = lines(5).replace("If false: throw to monkey ", "").toInt
    new Monkey(num, itemsListBuffer, operation, divisibleBy, trueTo, falseTo)
  }
}

case class Operation(op: String, by: String) {
  def compute(old: Long): Long = {
    val asLong = if (by == "old") old else by.toLong
    if (op == "+") {
      old + asLong
    } else {
      old * asLong
    }
  }
}
