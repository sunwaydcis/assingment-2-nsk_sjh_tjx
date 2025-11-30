import scala.io.Source
import IndicatorAnalysis.Row

object Main {

  def loadData(filePath: String): List[Row] = {

    val src = Source.fromFile (filePath, "latin1")

    try{
      val lines = src.getLines()

      if (!lines.hasNext) {
        Nil
      } else {

        val headerLine = lines.next()
        val headers: Array[String] =
          headerLine.split(",").map(_.trim)

        lines.toList.map { line =>
          val cols = line.split(",").map(_.trim)
          headers.zipAll(cols, "", "").toMap
        }
      }
    } finally {
      src.close()
    }
  }

  def main(args: Array[String]): Unit ={
    val filePath = "data/Hotel_Dataset.csv"

    val data = loadData(filePath)

    if (data.isEmpty) {
      println("No data found.")
      return
    }

    val analyses: List[IndicatorAnalysis] = List(
      new Q1_HighestBookingCountry
    )

    analyses.foreach(_.analyze(data))
  }

}
