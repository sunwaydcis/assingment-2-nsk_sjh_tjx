object Utils {

  def safeToDouble(str: String): Double =
    try str.toDouble
    catch {case _: Throwable => 0.0}
    
  def safeToInt(str: String): Int =
    try str.toInt
    catch {case _: Throwable => 0}

}
