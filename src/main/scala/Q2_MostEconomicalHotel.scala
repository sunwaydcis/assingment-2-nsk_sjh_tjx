import Utils._
import IndicatorAnalysis.Row

class Q2_MostEconomicalHotel extends IndicatorAnalysis {

  private val CountryKey = "Destination Country"
  private val CityKey = "Destination City"
  private val HotelNameKey = "Hotel Name"
  private val BookingPriceKey = "Booking Price[SGD]"
  private val DiscountKey = "Discount"
  private val ProfitMarginKey = "Profit Margin"

  override def analyze(data: List[Row]): Unit = {

    val validRows = data.filter { row =>
      row.getOrElse(CountryKey, "").nonEmpty &&
        row.getOrElse(CityKey, "").nonEmpty &&
        row.getOrElse(HotelNameKey, "").nonEmpty &&
        row.getOrElse(BookingPriceKey, "").nonEmpty &&
        row.getOrElse(DiscountKey, "").nonEmpty &&
        row.getOrElse(ProfitMarginKey, "").nonEmpty
    }

    if (validRows.isEmpty) {
      printNoData()
      return
    }


    val groupedByHotel: Map[(String, String, String), List[Row]] =
      validRows.groupBy(row => (row(CountryKey), row(CityKey), row(HotelNameKey)))

    val hotelMetrics: Map[(String, String, String), (Double, Double, Double)] =
        groupedByHotel.view.flatMap { case (key, rows) =>
          val parsed = rows.flatMap { row =>
            val priceStr = row.getOrElse(BookingPriceKey, "")
            val discountStr = row.getOrElse(DiscountKey, "")
            val marginStr = row.getOrElse(ProfitMarginKey, "")

            val price = safeToDouble(priceStr)
            val discountFrac = parsePercent(discountStr)
            val profitMargin = safeToDouble(marginStr)

            if (price <= 0) None
            else 
              Some((price, discountFrac, profitMargin))
          }

          if (parsed.isEmpty) {
            None
          } else {
            val count = parsed.size.toDouble
            val sumPrice = parsed.map(_._1).sum
            val sumDiscount = parsed.map(_._2).sum
            val sumMargin = parsed.map(_._3).sum

            val avgPricePerRoom = sumPrice / count
            val avgDiscount = sumDiscount / count
            val avgMargin = sumMargin / count

            Some(key -> (avgPricePerRoom, avgDiscount, avgMargin))
          }
        }.toMap

      if (hotelMetrics.isEmpty) {
        printNoData()
        return
      }

      val prices = hotelMetrics.values.map(_._1)
      val discounts = hotelMetrics.values.map(_._2)
      val margins = hotelMetrics.values.map(_._3)

      val minPrice = prices.min
      val maxPrice = prices.max
      val minDisc = discounts.min
      val maxDisc = discounts.max
      val minMarg = margins.min
      val maxMarg = margins.max

      def normalize(value: Double, min: Double, max: Double): Double = {
        if (max == min) 50.0 else (value - min) / (max - min) * 100.0
      }

      val hotelScores: Map[(String, String, String), Double] =
        hotelMetrics.map { case (key, (avgPrice, avgDisc, avgMarg)) =>
          val pricePct = normalize(avgPrice, minPrice, maxPrice)
          val priceScore = 100.0 - pricePct

          val discountPct = normalize(avgDisc, minDisc, maxDisc)
          val discountScore = discountPct

          val marginPct = normalize(avgMarg, minMarg, maxMarg)
          val marginScore = 100.0 - marginPct

          val finalScore = (priceScore + discountScore + marginScore) / 3.0
          key -> finalScore
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
    println("┌─────────────────────────────────────────────┐")
    println("│          ECONOMICAL HOTEL ANALYSIS          │")
    println("├─────────────────────────────────────────────┤")
    println("│        No valid hotel data was found.       │")
    println("└─────────────────────────────────────────────┘")
  }


  private def printResult(country: String, city: String, hotel: String, score: Double): Unit = {
    println("┌─────────────────────────────────────────────────┐")
    println("│            ECONOMICAL HOTEL ANALYSIS            │")
    println("├─────────────────────────────────────────────────┤")
    println(f"│    Country : $country                               │")
    println(f"│    City : $city%-20s                  │")
    println(f"│    Hotel : $hotel%-20s                 │")
    println(f"│    Economical Score : ${score}%.2f                     │")
    println("└─────────────────────────────────────────────────┘")
    println("(Higher Score= More economical overall, combining low price per room, higher discount and lower profit margin)")
  }
}