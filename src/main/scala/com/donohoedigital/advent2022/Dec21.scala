package com.donohoedigital.advent2022

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

// https://adventofcode.com/2022/day/21
object Dec21 extends App {

  val sample =
    """root: pppw + sjmn
      |dbpl: 5
      |cczh: sllz + lgvd
      |zczc: 2
      |ptdq: humn - dvpt
      |dvpt: 3
      |lfqf: 4
      |humn: 5
      |ljgn: 2
      |sjmn: drzm * dbpl
      |sllz: 4
      |pppw: cczh / lfqf
      |lgvd: ljgn * ptdq
      |drzm: hmdt - zczc
      |hmdt: 32""".stripMargin

  var input = Input.readFile("day21.txt", trim = false).split("\n")

  // uncomment to use sample data
  //input = sample.split("\n")

  // Input looks like:
  //   pmqg: 2
  //   pvcl: nnqq * svhn
  def monkeys = input.map { line =>
    val p1 = "(....): (\\d+)".r
    val p2 = "(....): (....) (.) (....)".r
    if (p1.matches(line)) {
      val p1(name, num) = line
      Monkey21(name, num.toInt, "", "", "")
    } else {
      val p2(name, o1, op, o2) = line
      Monkey21(name, 0, o1, op, o2)
    }
  }

  //println(monkeys.mkString("\n"))

  // Part 1 - correct answer is 31017034894002
  val p1 = Monkeys(monkeys)
  println("ROOT full equation:\n" + p1.root)
  p1.solvePart1()
  println("PART 1 ROOT: " + p1.root.value().get)
  println

  // Part 2 - correct answer is 3555057453229
  println(p1.rootInfo)
  val human = p1.solvePart2()
  println("PART 2 - new human value: " + human)
  // verify
  val p2 = Monkeys(monkeys)
  p2.human.num = human
  p2.solvePart1()
  println(p2.rootInfo)
}

case class Monkeys(monkeys: Array[Monkey21]) {

  // create map from monkey name to monkey
  val lookup = {
    val map = new mutable.HashMap[String, Monkey21]()
    map.addAll(monkeys.map(m => m.name -> m))
    map
  }

  // resolve operand names to actual monkeys + track dependents
  monkeys.foreach { monkey =>
    if (monkey.isOperation) {
      val m1 = lookup(monkey.o1)
      val m2 = lookup(monkey.o2)
      monkey.m1 = m1
      monkey.m2 = m2
      m1.addDependant(monkey)
      m2.addDependant(monkey)
    }
  }

  val root = lookup("root")
  val human = lookup("humn")
  human.markHumanPath()

  def solvePart1() = {
    // find nodes where we know the value and propagate it up to those that depend on it
    // repeat until we have one node left, the root
    while(lookup.size > 1) {
      val hasValue = lookup.values.filter(_.value().nonEmpty)
      hasValue.foreach { m =>
        m.notifyDependents()
        lookup.remove(m.name)
      }
    }
  }

  def rootInfo: String = {
    "ROOT values: " +
      root.m1Value.get + " (on human path: " + root.m1.onHumanPath +
      ") ==? " +
      root.m2Value.get + " (on human path: " + root.m2.onHumanPath +
      ") human="+human.num +
      (if (root.m1Value.get == root.m2Value.get) " ** EQUAL ** " else " xx NOT EQUAL xx")
  }

  def solvePart2(): Long = {
    val monkeys = List(root.m1, root.m2)
    // we want to start adjusting the side that the human value influences
    val start = monkeys.find(_.onHumanPath).get
    // our new target value is from the opposite side
    val target = monkeys.find(!_.onHumanPath).get.value().get
    // recalculate until 'humn' reached
    start.recalculate(target)
  }
}

case class Monkey21(name: String, var num: Long, o1: String, op: String, o2: String) {

  var m1: Monkey21 = _
  var m2: Monkey21 = _
  var m1Value: Option[Long] = None
  var m2Value: Option[Long] = None
  var onHumanPath = false

  def isOperation : Boolean = op != ""

  val dependsOnMe = new ListBuffer[Monkey21]
  def addDependant(monkey: Monkey21): Unit = {
    dependsOnMe += monkey
  }

  def notifyDependents() = {
    dependsOnMe.foreach(d => d.setValue(this, value().get))
  }

  def markHumanPath() : Unit = {
    onHumanPath = true
    dependsOnMe.foreach(d => d.markHumanPath())
  }

  def setValue(monkey: Monkey21, value: Long) = {
    if (monkey == m1){
      m1Value = Some(value)
    } else if (monkey == m2) {
      m2Value = Some(value)
    } else {
      throw new RuntimeException(this.name + " doesn't know about " + monkey.name)
    }
  }

  def value(): Option[Long] = {
    if (!isOperation) {
      Some(num)
    } else {
      if (m1Value.isEmpty || m2Value.isEmpty) {
        None
      } else {
        op match {
          case "+" => Some(m1Value.get + m2Value.get)
          case "-" => Some(m1Value.get - m2Value.get)
          case "*" => Some(m1Value.get * m2Value.get)
          case "/" => Some(m1Value.get / m2Value.get)
        }
      }
    }
  }

  // recalculate until the 'humn' is reached, return desired value
  def recalculate(desired: Long): Long = {
    if (name == "humn"){
      return desired
    }

    // reverse the above equations, solving for the monkey on the path to the human's value
    if (m1.onHumanPath) {
      op match {
        case "+" => m1.recalculate(desired - m2Value.get)
        case "-" => m1.recalculate(desired + m2Value.get)
        case "*" => m1.recalculate(desired / m2Value.get)
        case "/" => m1.recalculate(desired * m2Value.get)
      }
    } else if (m2.onHumanPath) {
      op match {
        case "+" => m2.recalculate(desired - m1Value.get)
        case "-" => m2.recalculate(m1Value.get - desired)
        case "*" => m2.recalculate(desired / m1Value.get)
        case "/" => m2.recalculate(m1Value.get / desired)
      }
    } else {
      throw new RuntimeException("Nobody on human path m1="+m1+" m2="+m2)
    }
  }

  override def toString: String = {
    toStringInternal.grouped(120).mkString("\n")
  }

  def toStringInternal: String = {
    if (isOperation) {
      "(" + m1.toStringInternal + " " + op + " " + m2.toStringInternal + ")"
    } else {
      if (name == "humn") name else num.toString
    }
  }
}