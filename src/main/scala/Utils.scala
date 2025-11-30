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
