object Utils {

  def safeToDouble(str: String): Double =
    try str.toDouble
    catch {case _: Throwable => 0.0}

  def safeToInt(str: String): Int =
    try str.toInt
    catch {case _: Throwable => 0}

  def topByFrequency[T](values: Iterable[T]): Option[(T, Int)] =
    values
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .maxByOption(_._2)

}

def parsePercent(str: String): Double ={
  val cleaned = str.replace("%","").trim
  if (cleaned.isEmpty) 0.0
  else safeToDouble(cleaned)/10.0
}

