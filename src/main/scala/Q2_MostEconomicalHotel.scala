import Utils._
import IndicatorAnalysis.Row

class Q2_MostEconomicalHotel extends IndicatorAnalysis {

private val HotelNameKey = "Hotel Name"
private val BookingPriceKey = "Booking Price[SGD]"
private val DiscountKey = "Discount"
private val ProfitMarginKey = "Profit Margin"

override def analyze(data: List[Row]): Unit = {

  val validRows = data.filter { row =>
    row.getOrElse(HotelNameKey, "").nonEmpty &&
      row.getOrElse(BookingPriceKey, "").nonEmpty &&
      row.getOrElse(DiscountKey, "").nonEmpty &&
      row.getOrElse(ProfitMarginKey, "").nonEmpty
  }

  val groupedByHotel: Map[String, List[Row]] =
    validRows.groupBy(row => row(HotelNameKey))

  val hotelScores: Map[String, Double] = {
    val scoredView = groupedByHotel.view.mapValues { rows =>
      val scores = rows.flatMap { row =>
        val priceStr = row.getOrElse(BookingPriceKey, "")
        val discountStr = row.getOrElse(DiscountKey, "")
        val marginStr = row.getOrElse(ProfitMarginKey, "")

        val price = safeToDouble(priceStr)
        val discountFrac = parsePercent(discountStr)
        val profitMargin = safeToDouble(marginStr)

        if (price <= 0) None
        else Some(computeEconomyScore(price, discountFrac, profitMargin))
      }

      if (scores.isEmpty) Double.PositiveInfinity
      else scores.sum / scores.size
    }

    scoredView.toMap
}

    val bestHotelOpt: Option[(String, Double)] =
      hotelScores.minByOption(_._2)

  bestHotelOpt match {
    case Some((hotel, score)) =>
      printResult(hotel, score)
    case None =>
      println("┌─────────────────────────────────────────────┐")
      println("│          ECONOMICAL HOTEL ANALYSIS          │")
      println("├─────────────────────────────────────────────┤")
      println("│        No valid hotel data was found.       │")
      println("└─────────────────────────────────────────────┘")
  }
}

  private def printResult(hotel: String, score: Double): Unit = {
    println("┌─────────────────────────────────────────────────┐")
    println("│            ECONOMICAL HOTEL ANALYSIS            │")
    println("├─────────────────────────────────────────────────┤")
    println(f"│    Most Economical Hotel : $hotel%-18s   │")
    println(f"│   Economical Score (lower is better): ${score}%.2f    │")
    println("└─────────────────────────────────────────────────┘")
  }
}