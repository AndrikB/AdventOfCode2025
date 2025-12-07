package day7

import utils.ReadFile

import scala.collection.mutable

object Day7 extends App {
  private val input = ReadFile.getLines("day7", "input.txt")
  //  private val input = ReadFile.getLines("day7", "testInput.txt")
  private val startPoint = input.head.indexOf("S")

  part1()
  part2()

  private def part1(): Unit = {
    var beams = mutable.Set(startPoint)
    var splitAmount = 0
    input.indices.foreach(i =>
      beams = beams.flatMap(j => {
        if (input(i)(j) == '^') {
          splitAmount += 1
          Seq(j - 1, j + 1)
        }
        else Seq(j)
      })
    )
    println(splitAmount)
  }

  private def part2(): Unit = {
    var beams = Map(startPoint -> BigInt(1))
    input.indices.foreach(i =>
      beams = beams
        .toSeq
        .flatMap((j, amount) => {
          if (input(i)(j) == '^') {
            Seq((j - 1, amount), (j + 1, amount))
          }
          else Seq((j, amount))
        })
        .groupMapReduce(_._1)(_._2)(_ + _)
    )
    println(beams.values.sum)
  }

}
