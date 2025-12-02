package utils

object ReadFile {
  def getLines(day: String = "day1", fileName: String): Seq[String] = {
    val filePath = s"src/main/scala/$day/$fileName"
    println(filePath)
    val f = scala.io.Source.fromFile(filePath)
    f.getLines().toSeq
  }
}