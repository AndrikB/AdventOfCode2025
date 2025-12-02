package day2

import utils.ReadFile

object Day2 extends App {
  private val input = ReadFile.getLines("day2", "input.txt")
    //  private val input = ReadFile.getLines("day2", "testInput.txt")
    .mkString("")
    .split(",")
    .map(_.split("-"))
    .map { arr => Range.Long.inclusive(arr(0).toLong, arr(1).toLong, 1) }

  part1()
  part2()

  private def part1(): Unit = {
    def invalidValue(elem: Long): Boolean = {
      val str = elem.toString
      str.matches("^(\\d+)\\1$")
    }

    val sum = input.map(
      range => range.filter(invalidValue).map(BigInt(_)).sum
    ).sum
    println(sum)
  }

  private def part2(): Unit = {
    def invalidValue(elem: Long): Boolean = {
      val str = elem.toString
      str.matches("^(\\d+)\\1+$")
    }

    val sum = input.map(
      range => range.filter(invalidValue).map(BigInt(_)).sum
    ).sum

    println(sum)
  }

}
