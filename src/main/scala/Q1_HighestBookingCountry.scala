class Q1_HighestBookingCountry extends IndicatorAnalysis{

  private val DestinationKey = "Destination Country"

  override def analyze(data: List[Map[String, String]]): Unit ={

    // Keep only rows where destination country is present

    val validRows = data.filter { row =>
      row.getOrElse(DestinationKey, "").nonEmpty
    }

    // Group bookings by destination country

    val grouped: Map[String, List[Map[String, String]]] =
      validRows.groupBy (row => row.getOrElse(DestinationKey, "Unknown"))

    //Count bookings per country

    val countrybycountry: Map[String, Int] =
      grouped.view.mapValues(_.size).toMap
  }

}
