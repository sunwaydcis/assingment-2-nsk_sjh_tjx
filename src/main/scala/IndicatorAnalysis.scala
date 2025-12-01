object IndicatorAnalysis {

  type Row = Map[String, String]

}

import IndicatorAnalysis.Row

trait IndicatorAnalysis {

  def analyze(data: List[Row]): Unit

}