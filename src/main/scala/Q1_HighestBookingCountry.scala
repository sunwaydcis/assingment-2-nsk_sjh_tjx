import Utils._
import IndicatorAnalysis.Row

class Q1_HighestBookingCountry extends IndicatorAnalysis{

  private val DestinationKey = "Destination Country"

  override def analyze(data: List[Row]): Unit = {

    val destinationCountries =

      data.view
        .flatMap(_.get(DestinationKey))
        .filter(_.nonEmpty)

    val topCountryOpt = topByFrequency(destinationCountries)

    println("[Country Booking Analysis]")

    topCountryOpt match {
      case Some((country, count)) =>
        println(s"Country: $country")
        println(s"Number of bookings: $count")

      case None =>
        println("No valid booking data found.")
    }
  }

}
