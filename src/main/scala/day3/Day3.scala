package day3

import utils.ReadFile

import scala.annotation.tailrec

object Day3 extends App {
  private val input = ReadFile.getLines("day3", "input.txt")
    //  private val input = ReadFile.getLines("day3", "testInput.txt")
    .map(line => line.toCharArray.toSeq.map(c => c - '0'))

  part1()
  part2()

  @tailrec
  private def getMaxVoltage(line: Seq[Int], amount: Int, result: BigInt = 0): BigInt = {
    if (amount == 0) return result
    val maxFirst = line.slice(0, line.size - amount + 1).max
    getMaxVoltage(line.slice(line.indexOf(maxFirst) + 1, line.size), amount - 1, result * 10 + maxFirst)
  }

  private def part1(): Unit = {
    println(input.map(line => getMaxVoltage(line, 2)).sum)

  }

  private def part2(): Unit = {
    println(input.map(line => getMaxVoltage(line, 12)).sum)
  }

}
