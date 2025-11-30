import Utils._
import IndicatorAnalysis.Row

class Q2_MostEconomicalHotel extends IndicatorAnalysis {
  
private val HotelNameKey = "Hotel Name"
private val BookingPriceKey = "Booking Price"
private val DiscountKey = "Discount"
private val ProfitMarginKey = "Profit Margin"

override def analyze(data: List[Row]): Unit = {
  
val validRows = data.filter { row =>
  row.getOrElse(HotelNameKey, "").nonEmpty && 
  row.getOrElse(BookingPriceKey, "").nonEmpty && 
  row.getOrElse(DiscountKey, "").nonEmpty
  row.getOrElse(ProfitMarginKey, "").nonEmpty
}

  