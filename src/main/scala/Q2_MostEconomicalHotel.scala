import Utils._
import IndicatorAnalysis.Row

class Q2_MostEconomicalHotel extends IndicatorAnalysis {

private val HotelNameKey = "Hotel Name"
private val BookingPriceKey = "Booking Price[SGD]"
private val DiscountKey = "Discount"
private val ProfitMarginKey = "Profit Margin"
private val PeopleOrRoomsKey = "No. Of People"

override def analyze(data: List[Row]): Unit = {

  val validRows = data.filter { row =>
    row.getOrElse(HotelNameKey, "").nonEmpty &&
      row.getOrElse(BookingPriceKey, "").nonEmpty &&
      row.getOrElse(DiscountKey, "").nonEmpty &&
      row.getOrElse(ProfitMarginKey, "").nonEmpty &&
      row.getOrElse(PeopleOrRoomsKey, "").nonEmpty
  }

   if (validRows.isEmpty) {
     printNoData()
     return
   }

  val groupedByHotel: Map[String, List[Row]] =
    validRows.groupBy(row => row(HotelNameKey))

  val hotelMetrics: Map[String, (Double, Double, Double)] =
    groupedByHotel.flatmap{ case (hotel,rows) =>
      val parsed = rows.flatMap { row =>
        val priceStr = row.getOrElse(BookingPriceKey, "")
        val discountStr = row.getOrElse(DiscountKey, "")
        val marginStr = row.getOrElse(ProfitMarginKey, "")
        val roomsStr = row.getOrElse(PeopleOrRoomsKey, "")

        val price = safeToDouble(priceStr)
        val discountFrac = parsePercent(discountStr)
        val profitMargin = safeToDouble(marginStr)
        val rooms = safeToInt(roomsStr)

        if (price <= 0 || rooms <=0) None
        else {
          val pricePerRoom = price / rooms.toDouble
          Some((pricePerRoom, discountFrac, profitMargin))
        }
      }

      if (parsed.isEmpty) {
        None
      }  else {
        val count = parsed.size.toDouble
        val sumPrice = parsed.map(_._1).sum
        val sumDiscount = parsed.map(_._2).sum
        val sumMargin = parsed.map(_._3).sum

        val avgPricePerRoom= sumPrice / count
        val avgDiscount = sumDiscount / count
        val avgMargin = sumMargin/count
        
        Some(hotel -> (avgPricePerRoom, avgDiscount, avgMargin))
    }
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
    println(f"│    Economical Score : ${score}%.2f                    │")
    println( "└─────────────────────────────────────────────────┘")
    println("(Lower score= Lower effective price after discount, which means a more economical option for customers.)")
  }

}