package day8

import utils.ReadFile

import scala.collection.mutable

object Day8 extends App {
  private val input = ReadFile.getLines("day8", "input.txt")
//  private val input = ReadFile.getLines("day8", "testInput.txt")
    .map(_.split(","))
    .map(arr => (arr(0).toInt, arr(1).toInt, arr(2).toInt))

  private val queue = new mutable.PriorityQueue[(Double, (Int, Int))]()((a, b) => b._1.compareTo(a._1))
  for (i <- input.indices) {
    for (j <- i + 1 until input.size) {
      queue.enqueue((length(i, j), (i, j)))
    }
  }

  part1()
  println("--------------------")
  part2()

  private def length(i: Int, j: Int): Double = math.sqrt(
    math.pow(input(i)._1 - input(j)._1, 2) +
      math.pow(input(i)._2 - input(j)._2, 2) +
      math.pow(input(i)._3 - input(j)._3, 2)
  )

  private def part1(): Unit = {
    val circles = mutable.HashMap[Int, Set[Int]]()
    input.indices.foreach(i => circles.put(i, Set(i)))

    queue.clone().dequeueAll.take(1000).foreach { case (size, (from, to)) =>
      val joined = circles(from) ++ circles(to)
      joined.foreach(circles(_) = joined)
    }

    val topCircuitsSize = circles.values.toSet.toSeq.map(_.size).sorted.reverse.take(3)

    println(topCircuitsSize)
    println(topCircuitsSize.product)
  }

  private def part2(): Unit = {
    val circles = mutable.HashMap[Int, Set[Int]]()
    input.indices.foreach(i => circles.put(i, Set(i)))

    var last = (0, 0)

    while (circles(0).size != input.size) {
      val (size, (from, to)) = queue.dequeue()

      val fromCircle = circles(from)
      val toCircle = circles(to)
      val joined = fromCircle ++ toCircle
      joined.foreach(
        circles(_) = joined
      )

      last = (from, to)
    }

    println(last)
    println(input(last._1)._1 * input(last._2)._1)
  }

}
