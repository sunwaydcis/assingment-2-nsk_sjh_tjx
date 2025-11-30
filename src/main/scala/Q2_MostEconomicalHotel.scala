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
  
  val groupedbyHotel: Map[String, List[Row]] =
    validRows.groupBy(row => row(HotelNameKey))
  
  val hotelScores: Map[String, Double] =
    groupedbyHotel.view.mapValues { rows =>
      val scores = rows.flatMap { row =>
        val priceStr = row.getOrElse(BookingPriceKey, "")
        val discountStr = row.getOrElse(DiscountKey, "")
        val marginStr = row.getOrElse(ProfitMarginKey, "")
        
        val price = safeToDouble(priceStr)
        val discountFrac = parcePercent(discountStr)
        val profitMargin = safeToDouble(marginStr)
        
        if (price < 0) None
        else Some(computeEconomyScore price, discountFrac, profitMargin)
      }
    
    
  