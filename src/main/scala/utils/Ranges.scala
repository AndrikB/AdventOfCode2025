package utils

import scala.collection.immutable.NumericRange

object Ranges {
  def joinRanges[T](ranges: Seq[NumericRange[T]])(implicit num: Integral[T]): Seq[NumericRange[T]] = {
    val sortedRanges = ranges.sortBy(_.start)

    val newRanges = sortedRanges.tail.foldLeft(List(sortedRanges.head)) { (mergedList, currentRange) =>
      val lastMerged = mergedList.head

      if (num.gt(currentRange.start, lastMerged.end)) {
        currentRange :: mergedList
      } else {
        val mergedRange = NumericRange(lastMerged.start, num.max(lastMerged.end, currentRange.end), num.fromInt(1))
        mergedRange :: mergedList.tail
      }
    }

    newRanges
  }

}
