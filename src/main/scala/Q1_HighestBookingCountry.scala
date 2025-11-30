class Q1_HighestBookingCountry extends IndicatorAnalysis{

  private val DestinationKey = "Destination Country"

  override def analyze(data: List[Map[String, String]]): Unit ={
    val validRows = data.filter { row =>
      row.getOrElse(DestinationKey, "")
    }
  }

}
