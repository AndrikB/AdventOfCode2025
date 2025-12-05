package day5

import utils.ReadFile

object Day5 extends App {
  private val input = ReadFile.getLines("day5", "input.txt")
  //    private val input = ReadFile.getLines("day5", "testInput.txt")

  private val freshIngredientRanges = input.takeWhile(_.nonEmpty)
    .map(_.split("-"))
    .map(_.map(BigInt(_)))
    .map(arr => Range.BigInt.inclusive(arr(0), arr(1), 1))

  private val ingredients = input.dropWhile(_.nonEmpty)
    .drop(1)
    .map(BigInt(_))

  part1()
  part2()

  private def part1(): Unit = {
    println(ingredients.count(f => freshIngredientRanges.exists(r => r.start <= f && r.end > f)))
  }

  private def part2(): Unit = {
    val sortedRanges = freshIngredientRanges.sortBy(_.start)

    val newRanges = sortedRanges.tail.foldLeft(List(sortedRanges.head)) { (mergedList, currentRange) =>
      val lastMerged = mergedList.head

      if (currentRange.start > lastMerged.end) {
        currentRange :: mergedList
      } else {
        val mergedRange = Range.BigInt.inclusive(lastMerged.start, lastMerged.end.max(currentRange.end), 1)
        mergedRange :: mergedList.tail
      }
    }
    val rangesSize = newRanges.map(range => range.end - range.start + 1).foldLeft(BigInt(0))((b, v) => b + v)

    println(rangesSize)
  }
}
