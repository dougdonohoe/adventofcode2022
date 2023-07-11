package com.donohoedigital.advent2022

import scala.io.Source

object Input {
  def readFile(name: String, trim: Boolean = true): String = {
    val buf = Source.fromResource("advent/" + name)
    val contents = buf.mkString
    buf.close()
    if (trim) contents.trim else contents
  }
}
