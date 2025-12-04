package day4

import utils.ReadFile

object Day4 extends App {
  private val input = ReadFile.getLines("day4", "input.txt")
    //  private val input = ReadFile.getLines("day4", "testInput.txt")
    .map(line => line.toCharArray)
  
  part1()
  part2()

  private def tryIndexes(x: Int, y: Int): Boolean = {
    var count = 1

    val neighbours = Seq[(Int, Int)](
      (x - 1) -> (y - 1),
      x - 1 -> y,
      x - 1 -> (y + 1),
      x -> (y - 1),
      x -> (y + 1),
      x + 1 -> (y - 1),
      x + 1 -> y,
      x + 1 -> (y + 1),
    )

    count += neighbours
      .filter((x, y) => x >= 0 && x < input.size && y >= 0 && y < input.head.length)
      .count((x, y) => input(x)(y) == '@')


    count < 5
  }

  private def part1(): Unit = {
    var count = 0

    input.indices.foreach(x =>
      input.head.indices.foreach(y =>

        if (input(x)(y) == '@' && tryIndexes(x, y)) {
          count += 1
        }

      )
    )

    println(count)
  }

  private def part2(): Unit = {
    var amount = 0
    var amountBefore = -1

    while (amount != amountBefore) {
      amountBefore = amount

      input.indices.foreach(x =>
        input.head.indices.foreach(y => {

          if (input(x)(y) == '@' && tryIndexes(x, y)) {
            input(x)(y) = '.'
            amount += 1
          }

        })
      )

    }

    println(amount)
  }


}
