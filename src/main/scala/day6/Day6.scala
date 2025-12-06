package day6

import utils.ReadFile

import scala.collection.mutable

object Day6 extends App {
  private val input = ReadFile.getLines("day6", "input.txt")
  //  private val input = ReadFile.getLines(я"day6", "testInput.txt")


  part1()
  part2()

  private def part1(): Unit = {
    val numbers = input.take(input.length - 1).map(line => line.split("\\s+").filter(_.nonEmpty).map(BigInt(_)))
    val operations = input.last.split("\\s+")

    println(operations.indices.map(i =>
      operations(i) match {
        case "+" => numbers.map(_(i)).sum
        case "*" => numbers.map(_(i)).foldLeft(BigInt(1))(_ * _)
      }).sum)
  }


  private def part2(): Unit = {
    val numbers = input.take(input.length - 1)
    val operationsMap = input.last.zipWithIndex.filter((c, _) => !c.isSpaceChar)

    def mapToNumber(i: Int): Seq[BigInt] = {
      var res = mutable.ArrayBuffer[String]()
      var j = i

      while (j < numbers.head.length) {
        val newNumber = numbers.map(_(j)).filter(_.isDigit).mkString("")

        if (newNumber.isEmpty) return res.map(BigInt(_)).toSeq

        res = res :+ newNumber
        j += 1
      }
      res.map(BigInt(_)).toSeq
    }

    println(operationsMap.map((c, i) =>
      c match {
        case '+' => mapToNumber(i).sum
        case '*' => mapToNumber(i).foldLeft(BigInt(1))(_ * _)
      }).sum)
  }
}