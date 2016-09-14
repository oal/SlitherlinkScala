object Utils {
  def toInt(s: String): Int = {
    try {
      s.toInt
    } catch {
      case e: Exception => -1
    }
  }
}