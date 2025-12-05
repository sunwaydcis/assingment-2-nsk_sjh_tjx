import Utils._
import IndicatorAnalysis.Row

class Q3_MostProfitableHotel extends IndicatorAnalysis {

  private val CountryKey      = "Destination Country"
  private val CityKey         = "Destination City"
  private val HotelNameKey    = "Hotel Name"
  private val VisitorsKey     = "No. Of People"
  private val ProfitMarginKey = "Profit Margin"

  override def analyze(data: List[Row]): Unit = {
    val validRows = data.filter { row =>
      row.getOrElse(CountryKey, "").nonEmpty &&
        row.getOrElse(CityKey, "").nonEmpty &&
        row.getOrElse(HotelNameKey, "").nonEmpty &&
        row.getOrElse(VisitorsKey, "").nonEmpty &&
        row.getOrElse(ProfitMarginKey, "").nonEmpty
    }

    if (validRows.isEmpty) {
      printNoData()
      return
    }

    val groupedByHotel: Map[(String, String, String), List[Row]] =
      validRows.groupBy(row =>
        (row(CountryKey), row(CityKey), row(HotelNameKey))
      )

    val hotelMetrics: Map[(String, String, String), (Int, Double)] =
      groupedByHotel.view.flatMap { case (key, rows) =>
        val parsed: List[(Int, Double)] = rows.flatMap { row =>
          val visitorsStr     = row.getOrElse(VisitorsKey, "0")
          val profitMarginStr = row.getOrElse(ProfitMarginKey, "0")

          val visitors     = safeToInt(visitorsStr)
          val profitMargin = safeToDouble(profitMarginStr)

          if (visitors <= 0 || profitMargin <= 0.0) None
          else Some((visitors, profitMargin))
        }

        if (parsed.isEmpty) None
        else {
          val totalVisitors = parsed.map(_._1).sum
          val avgMargin     = parsed.map(_._2).sum / parsed.size.toDouble
          Some(key -> (totalVisitors, avgMargin))
        }
      }.toMap

    if (hotelMetrics.isEmpty) {
      printNoData()
      return
    }

    val visitorTotals = hotelMetrics.values.map(_._1)
    val margins       = hotelMetrics.values.map(_._2)

    val minVisitors = visitorTotals.min.toDouble
    val maxVisitors = visitorTotals.max.toDouble
    val minMargin   = margins.min
    val maxMargin   = margins.max

    def normalize(value: Double, min: Double, max: Double): Double =
      if (max == min) 50.0 else (value - min) / (max - min) * 100.0

    val hotelScores: Map[(String, String, String), Double] =
      hotelMetrics.map { case (key, (totalVisitors, avgMargin)) =>
        val visitorScore = normalize(totalVisitors.toDouble, minVisitors, maxVisitors)
        val marginScore  = normalize(avgMargin, minMargin, maxMargin)
        val combinedScore = computeProfitScore(visitorScore, marginScore)
        key -> combinedScore
      }

    val bestHotelOpt: Option[((String, String, String), Double)] =
      hotelScores.maxByOption(_._2)

    bestHotelOpt match {
      case Some(((country, city, hotel), score)) =>
        printResult(country, city, hotel, score)
      case None =>
        printNoData()
    }
  }

  private def printNoData(): Unit = {
    println("┌────────────────────────────────────────────┐")
    println("│           PROFITABILITY ANALYSIS           │")
    println("├────────────────────────────────────────────┤")
    println("│       No valid hotel data was found.       │")
    println("└────────────────────────────────────────────┘")
  }

  private def printResult(country: String, city: String, hotel: String, score: Double): Unit = {
    println("┌──────────────────────────────────────┐")
    println("│       PROFITABILITY ANALYSIS         │")
    println("├──────────────────────────────────────┤")
    println(f"│     Hotel Name     : $hotel%-12s    │")
    println(f"│     Country        : $country%-12s    │")
    println(f"│     City           : $city%-12s    │")
    println(f"│     Combined Score : $score%.2f           │")
    println("└──────────────────────────────────────┘")
    println("(Higher score = more profitable overall based on total visitors and average profit margin)")
  }
}