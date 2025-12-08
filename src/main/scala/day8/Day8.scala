package day8

import utils.ReadFile

import scala.collection.mutable

object Day8 extends App {
  private val input = ReadFile.getLines("day8", "input.txt")
//  private val input = ReadFile.getLines("day8", "testInput.txt")
    .map(_.split(","))
    .map(arr => (arr(0).toInt, arr(1).toInt, arr(2).toInt))

  part1()
  println("--------------------")
  part2()

  private def length(i: Int, j: Int): Double = math.sqrt(
    math.pow(input(i)._1 - input(j)._1, 2) +
      math.pow(input(i)._2 - input(j)._2, 2) +
      math.pow(input(i)._3 - input(j)._3, 2)
  )

  private def part1(): Unit = {
    val queue = LimitedSizePriorityQueue[(Double, (Int, Int))](1000)((a, b) => a._1.compareTo(b._1))
    for (i <- input.indices) {
      for (j <- i + 1 until input.size) {
        queue.enqueue((length(i, j), (i, j)))
      }
    }

//    println(queue)

    val circles = mutable.HashMap[Int, Set[Int]]()
    input.indices.foreach(i => circles.put(i, Set(i)))

    queue.toSeq.foreach {
      case (size, (from, to)) =>
        val fromCircle = circles(from)
        val toCircle = circles(to)
        val joined = fromCircle ++ toCircle
        joined.foreach(
          circles(_) = joined
        )
    }

//    println(queue)

//    println(circles)
//    println(circles.values.toSet.toSeq.map(_.size))

    val topCircuitsSize = circles.values.toSet.toSeq.map(_.size).sorted.reverse.take(3)

    println(topCircuitsSize)
    println(topCircuitsSize.product)
  }

  private def part2(): Unit = {
    val queue = new mutable.PriorityQueue[(Double, (Int, Int))]()((a, b) => b._1.compareTo(a._1))
    for (i <- input.indices) {
      for (j <- i + 1 until input.size) {
        queue.enqueue((length(i, j), (i, j)))
      }
    }

//    println(queue)

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

class LimitedSizePriorityQueue[A](maxSize: Int)(implicit ord: Ordering[A]) {
  require(maxSize > 0, "Max size must be positive")

  private val underlyingQueue = new mutable.PriorityQueue[A]()

  def enqueue(elem: A): Unit = {
    if (underlyingQueue.size < maxSize) {
      underlyingQueue.enqueue(elem)
    } else {
      if (ord.compare(elem, underlyingQueue.head) < 0) { // If elem is larger than the current largest (tail)
        underlyingQueue.dequeue() // Remove the largest
        underlyingQueue.enqueue(elem) // Add the smaller one
      }
    }
  }

  def toSeq: Seq[A] = underlyingQueue.toSeq

  def dequeue(): A = underlyingQueue.dequeue()

  def head: A = underlyingQueue.head

  def isEmpty: Boolean = underlyingQueue.isEmpty

  def size: Int = underlyingQueue.size

  override def toString: String = underlyingQueue.toString
}

