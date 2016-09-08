class Square(
              val x: Int,
              val y: Int,
              val number: Option[Int],
              val top: Option[Boolean] = None,
              val right: Option[Boolean] = None,
              val bottom: Option[Boolean] = None,
              val left: Option[Boolean] = None) {

  val solutions = Utils.genSolutions(number)
    .filter(s => top.isEmpty || s._1 == top.get)
    .filter(s => right.isEmpty || s._2 == right.get)
    .filter(s => bottom.isEmpty || s._3 == bottom.get)
    .filter(s => left.isEmpty || s._4 == left.get)

  def isSolved = solutions.length == 1

  def setTop(value: Boolean) = {
    new Square(x, y, number, Some(value), right, bottom, left)
  }

  def setRight(value: Boolean) = {
    new Square(x, y, number, top, Some(value), bottom, left)
  }

  def setBottom(value: Boolean) = {
    new Square(x, y, number, top, right, Some(value), left)
  }

  def setLeft(value: Boolean) = {
    new Square(x, y, number, top, right, bottom, Some(value))
  }

  def toLine = {
    if (top.getOrElse(false)) Some(List((x, y), (x + 1, y)))
    else if (right.getOrElse(false)) Some(List((x + 1, y), (x + 1, y + 1)))
    else if (bottom.getOrElse(false)) Some(List((x, y + 1), (x + 1, y + 1)))
    else if (left.getOrElse(false)) Some(List((x, y), (x, y + 1)))
    else None
  }

  override def toString: String = {
    s"""
[${x}x$y]
  Number: $number
  Sides:  [$top, $right, $bottom, $left]""".stripMargin
  }
}