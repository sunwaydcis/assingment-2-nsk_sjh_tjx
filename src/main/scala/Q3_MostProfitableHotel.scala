import Utils._
import IndicatorAnalysis.Row

class Q3_MostProfitableHotel extends IndicatorAnalysis {

  private val HotelNameKey = "Hotel Name"
  private val VisitorsKey = "Number of People"
  private val ProfitMarginKey  = "Profit Margin"

  override def analyze(data: List[Row]): Unit = {

    val validRows = data.filter { row =>
        row.getOrElse(HotelNameKey, "").nonEmpty &&
        row.getOrElse(VisitorsKey, "").nonEmpty 
    }
  }
}
