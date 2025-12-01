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
        row.getOrElse(ProfitMarginKey, "").nonEmpty &&
    }

    bestHotelOpt match {
      case Some((hotel, score)) =>
        printResult(hotel, score)
      case None =>
        printNoData()
    }
  }

    val hotelScores: Map[String, Double] =
      groupedByHotel.view.mapValues { rows =>
        val scores: List[Double] = rows.flatMap { row =>
          val visitorsStr = row.getOrElse(VisitorsKey, "0")
          val profitMarginStr = row.getOrElse(ProfitMarginKey, "")
          val visitors = safeToInt(visitorsStr)
          val profitMargin = safeToDouble(profitMargin)

          if (visitors <= 0 || profitMargin.isNaN)
          else Some(visitors.toDouble * profitMargin)
        }
      }
        
        private def printNoData(): Unit = {
          println("┌─────────────────────────────────────────┐")
          println("│          PROFITABILITY ANALYSIS        │")
          println("├─────────────────────────────────────────┤")
          println("│    No valid hotel data was found.      │")
          println("└─────────────────────────────────────────┘")
        }
        
        
        
}
