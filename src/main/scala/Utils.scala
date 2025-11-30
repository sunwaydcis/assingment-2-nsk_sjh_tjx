object Utils {

  def safeToDouble(str: String): Double =
    try str.toDouble
    catch {
      case _: Throwable => 0.0
    }

  def safeToInt(str: String): Int =
    try str.toInt
    catch {
      case _: Throwable => 0
    }

  def topByFrequency[T](values: Iterable[T]): Option[(T, Int)] =
    values
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .maxByOption(_._2)


  def parsePercent(str: String): Double = {
    val cleaned = str.replace("%", "").trim
    if (cleaned.isEmpty) 0.0
    else safeToDouble(cleaned) / 100.0
  }

  def computeEconomyScore(price: Double, discountFraction: Double, profitMargin: Double): Double = {
    val effectivePrice = price * (1.0 - discountFraction)
    effectivePrice * (1.0 + profitMargin)
  }
}