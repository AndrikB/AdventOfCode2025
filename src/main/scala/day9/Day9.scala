package day9

import utils.ReadFile


object Day9 extends App {
  private val input = ReadFile.getLines("day9", "input.txt")
    //  private val input = ReadFile.getLines("day9", "testInput.txt")
    .map(_.split(',').map(_.toInt))
    .map(arr => (arr(0), arr(1)))

  part1()
  println("--------------------")
  part2()


  private def part1(): Unit = {
    var maxArea = 0L
    for ((x1, y1) <- input) {
      for ((x2, y2) <- input) {
        val area = ((x1 - x2).abs + 1L) * ((y1 - y2).abs + 1)
        maxArea = area.max(maxArea)
      }
    }

    println(maxArea)
  }

  private def part2(): Unit = {
    val lines = input.zip(input.tail :+ input.head)

    val n = input.length

    def checkSquareHasIntersection(xMin: Int, xMax: Int, yMin: Int, yMax: Int): Boolean = {
      lines.exists { case ((x1, y1), (x2, y2)) =>
        //vertical line
        if (x1 == x2) {
          val bottom = y1.min(y2)
          val top = y1.max(y2)

          // In case X's from square are on different side of current line.
          xMin < x1 && x1 < xMax &&
            bottom.max(yMin) < top.min(yMax)
          // Take two vertical 'segment' (Y's from square and current line)
          // Check if maximum of  the lower points is higher than the minimum of higher points
        }
        //horizontal line
        else if (y1 == y2) {
          val left = x1.min(x2)
          val right = x1.max(x2)

          yMin < y1 && y1 < yMax &&
            left.max(xMin) < right.min(xMax)
        }
        else false
      }
    }

    var maxArea = 0L

    for (i <- input.indices) {
      val (x1, y1) = input(i)
      for (j <- i + 1 until n) {
        val (x2, y2) = input(j)
        
        val xMin = x1.min(x2)
        val xMax = x1.max(x2)
        val yMin = y1.min(y2)
        val yMax = y1.max(y2)

        val area = (xMax - xMin + 1L) * (yMax - yMin + 1)

        if (area > maxArea) {
          if (!checkSquareHasIntersection(xMin, xMax, yMin, yMax)) {
            maxArea = area
          }
        }
      }
    }

    println(maxArea)
  }

}
