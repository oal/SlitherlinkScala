class Board(width: Int, height: Int, numbers: List[List[Option[Int]]]) {
  var squares = (0 until height).map(y => {
    (0 until width).map(x => {
      new Square(x, y, numbers(y)(x))
    }).toArray
  }).toArray

  def copy() = {
    val b = new Board(width, height, numbers)
    b.squares = squares.map(s => s.clone())
    b
  }

  def getFirstSolved: Option[Square] = {
    (0 until height).foreach(y => {
      (0 until width).foreach(x => {
        val sq = getSquare(x, y)
        if (sq.isSolved) return Some(sq)
      })
    })

    None
  }

  def getSquare(x: Int, y: Int) = {
    squares(y)(x)
  }

  def setSquare(x: Int, y: Int, square: Square) = {
    squares(y)(x) = square
  }

  def setTop(x: Int, y: Int, value: Boolean) = {
    setSquare(x, y, getSquare(x, y).setTop(value))
    if (y > 0) setSquare(x, y - 1, getSquare(x, y - 1).setBottom(value))
  }

  def setRight(x: Int, y: Int, value: Boolean) = {
    setSquare(x, y, getSquare(x, y).setRight(value))
    if (x < width - 1) setSquare(x + 1, y, getSquare(x + 1, y).setLeft(value))
  }

  def setBottom(x: Int, y: Int, value: Boolean) = {
    setSquare(x, y, getSquare(x, y).setBottom(value))
    if (y < height - 1) setSquare(x, y + 1, getSquare(x, y + 1).setTop(value))
  }

  def setLeft(x: Int, y: Int, value: Boolean) = {
    setSquare(x, y, getSquare(x, y).setLeft(value))
    if (x > 0) setSquare(x - 1, y, getSquare(x - 1, y).setRight(value))
  }

  def getLineUp(x: Int, y: Int) = {
    if (y < 1) None
    else getSquare(x, y - 1).left
  }

  def getLineRight(x: Int, y: Int) = {
    if (x >= width - 1) None
    else getSquare(x, y).top
  }

  def getLineDown(x: Int, y: Int) = {
    if (y >= height - 1) None
    else getSquare(x, y).left
  }

  def getLineLeft(x: Int, y: Int) = {
    if (x < 1) None
    else getSquare(x - 1, y).top
  }

  def setLineUp(x: Int, y: Int, value: Boolean): Unit = {
    if (y < 1) None
    else setLeft(x, y - 1, value)
    if (value) {
      if (getLineLeft(x, y).getOrElse(false)) setLineLeft(x, y, false)
      if (getLineDown(x, y).getOrElse(false)) setLineDown(x, y, false)
      if (getLineRight(x, y).getOrElse(false)) setLineRight(x, y, false)
    }
  }

  def setLineRight(x: Int, y: Int, value: Boolean): Unit = {
    if (x < 1) None
    else setTop(x - 1, y, value)
    if (value) {
      if (getLineUp(x, y).getOrElse(false)) setLineUp(x, y, false)
      if (getLineLeft(x, y).getOrElse(false)) setLineLeft(x, y, false)
      if (getLineDown(x, y).getOrElse(false)) setLineDown(x, y, false)
    }
  }

  def setLineDown(x: Int, y: Int, value: Boolean): Unit = {
    if (y >= height - 1) None
    else setLeft(x, y, value)
    if (value) {
      if (getLineLeft(x, y).getOrElse(false)) setLineLeft(x, y, false)
      if (getLineUp(x, y).getOrElse(false)) setLineUp(x, y, false)
      println("RIGHT", getLineRight(x, y))
      if (getLineRight(x, y).getOrElse(false)) setLineRight(x, y, false)
    }
  }

  def setLineLeft(x: Int, y: Int, value: Boolean): Unit = {
    if (x < 1) None
    else setTop(x - 1, y, value)

    if (value) {
      if (getLineUp(x, y).getOrElse(false)) setLineUp(x, y, false)
      if (getLineRight(x, y).getOrElse(false)) setLineRight(x, y, false)
      if (getLineDown(x, y).getOrElse(false)) setLineDown(x, y, false)
    }
  }

  def isValid(): Boolean = {
    (0 until height).foreach(y => {
      (0 until width).foreach(x => {
        val sq = getSquare(x, y)
        if (!sq.isSolved) return false
      })
    })
    true
  }

  override def toString: String = {
    val lines = (0 until height).map(y => {
      val hLine = (0 until width).map(x => {
        val sq = getSquare(x, y)
        if (sq.top.getOrElse(false)) "-" else " "
      }).mkString("+")

      val vLine = (0 until width).map(x => {
        val sq = getSquare(x, y)
        if (sq.left.getOrElse(false)) "|" else " "
      }).mkString(" ")

      val vExtra = if (getSquare(width - 1, y).right.getOrElse(false)) "|" else " "
      s"+$hLine+\n$vLine $vExtra"
    }).mkString("\n")

    val hExtra = (0 until width).map(x => {
      val sq = getSquare(x, height - 1)
      if (sq.bottom.getOrElse(false)) "-" else " "
    }).mkString("+")

    s"\n${width}x${height}\n$lines\n+$hExtra+"
  }
}
