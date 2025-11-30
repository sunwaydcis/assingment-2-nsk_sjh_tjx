object Main {

  def loadData(filePath: String): List[Map[String, String]] = {
    
    val src = Source.fromFile (filePath, "latin1")
    
    try{
      val lines = src.getLines()
      
      if (!lines.hasNext) {
        Nil
      } else {
        
        val headerLine = lines.next()
        val headers: Array[String] =
          headerLine.split(",").map(_.trim)
          
        lines.toList.map { line =>
          val cols = line,split(",").map(_.trim)
          headers.zipAll(cols, ",").toMap
        }
      }
    } 
  }

}
