package com.donohoedigital.advent2022

import scala.collection.mutable.ListBuffer

case class File(name: String, size: Int) {
  def toString(indent: String): String = indent + "- " + name + " (file, size=" + size + ")"
}

case class Dir(name: String, parent: Dir, files: ListBuffer[File] = new ListBuffer[File],
               dirs: ListBuffer[Dir] = new ListBuffer[Dir]) {

  def size: Int = {
    files.map(_.size).sum + dirs.map(_.size).sum
  }

  def addFile(file: File): Unit = {
    files += file
  }

  def addDir(name: String): Dir = {
    val dir = Dir(name, this)
    dirs += dir
    dir
  }

  def cd(name: String): Dir = {
    if (name == "..") {
      parent
    } else {
      dirs.find(_.name == name).get
    }
  }

  override def toString: String = {
    toString("", "")
  }

  def toString(path: String, indent: String): String = {
    val fullPath = if (name == "/") "/" else indent + "- " + name

    val subIndent = indent + "  "
    val fileList = if (files.isEmpty) "" else files.map(_.toString(subIndent)).mkString("\n") + "\n"
    val dirList = dirs.map(_.toString(fullPath, subIndent)).mkString("")
    fullPath + " (dir) \n" + fileList + dirList
  }
}

object Dec7 {
  def main(args: Array[String]): Unit = {

    val root = Dir("/", null)
    val lines = Input.readFile("day7.txt", trim = false).split("\n")
    val allDirs = new ListBuffer[Dir]
    allDirs += root

    var pwd = root
    lines.foreach(line => {
      // cd
      if (line.startsWith("$ cd")) {
        val name = line.replace("$ cd ", "")
        name match {
          case "/" => pwd = root
          case s => pwd = pwd.cd(s)
        }
      }
      // new dir
      else if (line.startsWith("dir ")) {
        allDirs += pwd.addDir(line.replace("dir ", ""))
      }
      // ls - ignore
      else if (line.startsWith("$ ls")) {

      }
      // file
      else {
        val parts = line.split(" ")
        pwd.addFile(File(parts(1), parts(0).toInt))
      }
    })

    println(root)

    // Part 1: Find all of the directories with a total size of at most 100000.
    // What is the sum of the total sizes of those directories?
    // Correct answer: 1297683
    println("Part 1: " + allDirs.filter(_.size <= 100000).map(_.size).sum)

    // Part 2: Total available is 70000000
    // To run the update, you need unused space of at least 30000000.
    // Find the smallest directory that, if deleted, would free up enough space on the filesystem
    // to run the update. What is the total size of that directory?
    // Correct answer: 5756764
    val total = 70000000
    val unusedNeed = 30000000
    val totalFree = total - root.size
    val toFree = unusedNeed - totalFree
    println("Part 2: total used:        " + root.size)
    println("Part 2: total free:        " + totalFree)
    println("Part 2: needed to free:     " + toFree)
    println("Part 2: min (answer)        " + allDirs.filter(_.size > toFree).map(_.size).min)
  }
}
