class SquareList(val width: Int, val height: Int, squares: Array[Square]) {

  def isOnBoard(x: Int, y: Int) = !(x < 0 || y < 0 || x >= width || y >= height)

  def getSquare(x: Int, y: Int) = squares(y * width + x)

  def setSquare(x: Int, y: Int, square: Square) = squares(y * width + x) = square

  def applyRules() = {
    squares.foreach(s => println(s))
    println("\n\nOnes in corners")

    // Apply rules (independent of existing knowledge)
    onesInCorners()
    squares.foreach(s => println(s))
    println("\n\nTwos in corners")

    twosInCorners()
    squares.foreach(s => println(s))
    println("\n\nThrees in corners")

    threesInCorners()
    squares.foreach(s => println(s))
    println("\n")

    // Apply position dependent rules (none yet)
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        threeAboveZero(x, y)
        threeNextToThree(x, y)
      }
    }
  }

  // Rules
  def onesInCorners() = {
    if (getSquare(0, 0).number.contains(1)) {
      val filteredSquare = getSquare(0, 0).setTopKnown(false).setLeftKnown(false)
      setSquare(0, 0, filteredSquare)
    }
    if (getSquare(width - 1, 0).number.contains(1)) {
      val filteredSquare = getSquare(width - 1, 0).setTopKnown(false).setRightKnown(false)
      setSquare(width - 1, 0, filteredSquare)
    }
    if (getSquare(0, height - 1).number.contains(1)) {
      val filteredSquare = getSquare(0, height - 1).setBottomKnown(false).setLeftKnown(false)
      setSquare(0, height - 1, filteredSquare)
    }
    if (getSquare(width - 1, height - 1).number.contains(1)) {
      val filteredSquare = getSquare(width - 1, height - 1).setBottomKnown(false).setRightKnown(false)
      setSquare(width - 1, height - 1, filteredSquare)
    }
  }

  def twosInCorners() = {
    if (getSquare(0, 0).number.contains(2)) {
      setSquare(1, 0, getSquare(1, 0).setTopKnown(true))
      setSquare(0, 1, getSquare(0, 1).setLeftKnown(true))
    }
    if (getSquare(width - 1, 0).number.contains(2)) {
      setSquare(width - 2, 0, getSquare(width - 2, 0).setTopKnown(true))
      setSquare(width - 1, 1, getSquare(width - 1, 1).setRightKnown(true))
    }
    if (getSquare(0, height - 1).number.contains(2)) {
      setSquare(2, height - 1, getSquare(2, height - 1).setBottomKnown(true))
      setSquare(0, height - 2, getSquare(0, height - 2).setLeftKnown(true))
    }
    if (getSquare(width - 1, height - 1).number.contains(2)) {
      setSquare(width - 2, height - 1, getSquare(width - 2, height - 1).setBottomKnown(true))
      setSquare(width - 1, height - 2, getSquare(width - 1, height - 2).setRightKnown(true))
    }
  }

  def threesInCorners() = {
    if (getSquare(0, 0).number.contains(3)) {
      val topFiltered = getSquare(0, 0).setTopKnown(true)
      val topLeftFiltered = topFiltered.setLeftKnown(true)
      setSquare(0, 0, topLeftFiltered)
    }
    if (getSquare(width - 1, 0).number.contains(3)) {
      val topFiltered = getSquare(width - 1, 0).setTopKnown(true)
      val topRightFiltered = topFiltered.setRightKnown(true)
      setSquare(width - 1, 0, topRightFiltered)
    }
    if (getSquare(0, height - 1).number.contains(3)) {
      val bottomFiltered = getSquare(0, height - 1).setBottomKnown(true)
      val bottomLeftFiltered = bottomFiltered.setLeftKnown(true)
      setSquare(0, height - 1, bottomLeftFiltered)
    }
    if (getSquare(width - 1, height - 1).number.contains(3)) {
      val bottomFiltered = getSquare(width - 1, height - 1).setBottomKnown(true)
      val bottomRightFiltered = bottomFiltered.setRightKnown(true)
      setSquare(width - 1, height - 1, bottomRightFiltered)
    }
  }

  // Position dependent rules
  def threeAboveZero(x: Int, y: Int): Unit = {
    if (!(isOnBoard(x - 1, y) && isOnBoard(x + 1, y) && isOnBoard(x, y + 1))) return

    if (getSquare(x, y).number.contains(3) && getSquare(x, y + 1).number.contains(0)) {
      setSquare(x, y, getSquare(x, y).setTopKnown(true).setLeftKnown(true).setRightKnown(true))
      setSquare(x - 1, y, getSquare(x - 1, y).setBottomKnown(true).setRightKnown(true))
      setSquare(x + 1, y, getSquare(x + 1, y).setBottomKnown(true).setLeftKnown(true))
    }
  }

  def threeNextToThree(x: Int, y: Int): Unit = {
    if (isOnBoard(x + 1, y)) return

    if (getSquare(x, y).number.contains(3) && getSquare(x + 1, y).number.contains(3)) {
      setSquare(x, y, getSquare(x, y).setLeftKnown(true).setRightKnown(true))
      setSquare(x + 1, y, getSquare(x + 1, y).setLeftKnown(true).setRightKnown(true))

      if (y > 0) {
        setSquare(x, y - 1, getSquare(x, y - 1).setRightKnown(false))
        setSquare(x + 1, y - 1, getSquare(x + 1, y - 1).setLeftKnown(false))
      }

      if (y < height-2) {
        setSquare(x, y + 1, getSquare(x, y + 1).setRightKnown(false))
        setSquare(x + 1, y + 1, getSquare(x + 1, y + 1).setLeftKnown(false))
      }
    }
  }
}