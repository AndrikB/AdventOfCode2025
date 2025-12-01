package day1

import utils.ReadFile

object Day1 extends App {
  //  val input = ReadFile.getLines("day1", "testInput.txt")
  private val input = ReadFile.getLines("day1", "input.txt")
  private val ints = input.map(s => (if (s.charAt(0) == 'R') +1 else -1) * s.substring(1).toInt)

  part1()
  part2()

  private def part1(): Unit = {
    var sum = 50
    var count = 0

    for (elem <- ints) {
      sum = (sum + elem + 100) % 100
      if (sum == 0) {
        count += 1
      }
    }

    println(count)
  }

  private def part2(): Unit = {
    var sum = 50
    var sumBefore = 50
    var count = 0
    for (elem <- ints) {
      sumBefore = sum
      sum += elem

      if (sum < 0) {
        count += sum.abs / 100
        if (sumBefore != 0) {
          count += 1
        }
      }
      else if (sum == 0) {
        count += 1
      }
      else if (sum >= 100) {
        count += sum.abs / 100
      }

      sum = (sum % 100 + 100) % 100
    }
    println(count)
  }

}
