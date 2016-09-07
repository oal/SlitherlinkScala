object Utils {
  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }
  }

  def genSolutions(n: Option[Int]): List[(Boolean, Boolean, Boolean, Boolean)] = {
    n match {
      case Some(0) => List(
        (false, false, false, false)
      )
      case Some(1) => List(
        (true, false, false, false),
        (false, true, false, false),
        (false, false, true, false),
        (false, false, false, true)
      )
      case Some(2) => List(
        (true, true, false, false),
        (false, false, true, true),
        (true, false, true, false),
        (true, false, false, true),
        (false, true, false, true),
        (false, true, true, false)
      )
      case Some(3) => List(
        (true, true, true, false),
        (true, false, true, true),
        (true, true, false, true),
        (false, true, true, true)
      )
      case Some(4) => List(
        (true, true, true, true)
      )
      case _ => genSolutions(Some(0)) ++ genSolutions(Some(1)) ++ genSolutions(Some(2)) ++ genSolutions(Some(3)) ++ genSolutions(Some(4))
    }
  }
}