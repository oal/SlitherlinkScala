class Square(val x: Int, val y: Int, val number: Option[Int], val possibleSolutions: List[(Boolean, Boolean, Boolean, Boolean)]) {
  def isSolved = possibleSolutions.length == 1

  def solution = possibleSolutions.head

  def setTopKnown(state: Boolean) = new Square(x, y, number, possibleSolutions.filter(p => p._1 == state))

  def setRightKnown(state: Boolean) = new Square(x, y, number, possibleSolutions.filter(p => p._2 == state))

  def setBottomKnown(state: Boolean) = new Square(x, y, number, possibleSolutions.filter(p => p._3 == state))

  def setLeftKnown(state: Boolean) = new Square(x, y, number, possibleSolutions.filter(p => p._4 == state))

  def popSolution() = new Square(x, y, number, possibleSolutions.tail)

  def maybeTop = possibleSolutions.exists(p => p._1)

  def maybeRight = possibleSolutions.exists(p => p._2)

  def maybeBottom = possibleSolutions.exists(p => p._3)

  def maybeLeft = possibleSolutions.exists(p => p._4)

  override def toString: String = s"$x x $y (${possibleSolutions.length} solutions): $possibleSolutions"
}