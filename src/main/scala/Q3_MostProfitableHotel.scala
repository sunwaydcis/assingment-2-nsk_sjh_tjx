import Utils._
import IndicatorAnalysis.Row

class Q3_MostProfitableHotel extends IndicatorAnalysis {

  private val HotelNameKey = "Hotel Name"
  private val VisitorsKey = "No. Of People"
  private val ProfitMarginKey  = "Profit Margin"

  override def analyze(data: List[Row]): Unit = {

    val validRows = data.filter { row =>
      row.getOrElse(HotelNameKey, "").nonEmpty &&
        row.getOrElse(VisitorsKey, "").nonEmpty &&
        row.getOrElse(ProfitMarginKey, "").nonEmpty
    }

    if (validRows.isEmpty) {
      printNoData()
      return
    }
    val groupedByHotel: Map[String, List[Row]] =
      validRows.groupBy(row => row(HotelNameKey))

    val hotelScores: Map[String, Double] =
      groupedByHotel.view.mapValues { rows =>
        val scores: List[Double] = rows.flatMap { row =>
          val visitorsStr = row.getOrElse(VisitorsKey, "0")
          val profitMarginStr = row.getOrElse(ProfitMarginKey, "0")
          val visitors = safeToInt(visitorsStr)
          val profitMargin = safeToDouble(profitMarginStr)

          if (visitors <= 0 || profitMargin.isNaN) None
          else Some(computeProfitScore(visitors, profitMargin))
        }

        if (scores.isEmpty) Double.NegativeInfinity
        else scores.sum
      }.toMap

    val bestHotelOpt: Option[(String, Double)] =
      hotelScores.maxByOption(_._2)

    bestHotelOpt match {
      case Some((hotel, score)) =>
        printResult(hotel, score)
      case None =>
        printNoData()
    }
}

  private def printNoData(): Unit = {
    println("┌─────────────────────────────────────────┐")
    println("│          PROFITABILITY ANALYSIS         │")
    println("├─────────────────────────────────────────┤")
    println("│      No valid hotel data was found.     │")
    println("└─────────────────────────────────────────┘")
  }

  private def printResult(hotel: String, score: Double): Unit = {
    println("┌─────────────────────────────────────────────────┐")
    println("│              PROFITABILITY ANALYSIS             │")
    println("├─────────────────────────────────────────────────┤")
    println(f"│ Most Profitable Hotel : $hotel%-18s             │")
    println(f"│ Profit Score (visitors × margin): $score%8.2f   │")
    println("└─────────────────────────────────────────────────┘")
  }
}