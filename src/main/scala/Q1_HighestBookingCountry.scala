import Utils._
import IndicatorAnalysis.Row

class Q1_HighestBookingCountry extends IndicatorAnalysis {

  private val DestinationKey = "Destination Country"

  override def analyze(data: List[Row]): Unit = {

    val destinationCountries =
      
      data.view
        .flatMap(_.get(DestinationKey))
        .filter(_.nonEmpty)

    val topCountryOpt = topByFrequency(destinationCountries)

    topCountryOpt match {
      case Some((country, count)) =>
        printResult(country, count)
      case None =>
        println("┌─────────────────────────────────────────┐")
        println("│        COUNTRY BOOKING ANALYSIS         │")
        println("├─────────────────────────────────────────┤")
        println("│      No valid booking data found:(      │")
        println("└─────────────────────────────────────────┘")
    }
  }

  private def printResult(country: String, count: Int): Unit = {
    println("┌─────────────────────────────────────────┐")
    println("│        COUNTRY BOOKING ANALYSIS         │")
    println("├─────────────────────────────────────────┤")
    println(f"│  Highest Booking Country : $country%-12s │")
    println(f"│    Total Number of Bookings : $count%-11d │")
    println("└─────────────────────────────────────────┘")
  }

}
