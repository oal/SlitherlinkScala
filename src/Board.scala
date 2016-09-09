class Board(val width: Int,
            val height: Int,
            val numbers: List[List[Option[Int]]],
            var vertical: List[Array[Option[Boolean]]],
            var horizontal: List[Array[Option[Boolean]]]) {

  def copy() = {
    new Board(
      width, height, numbers,
      vertical.map(l => l.clone()),
      horizontal.map(l => l.clone())
    )
  }

  // Getters and setters
  def getTop(x: Int, y: Int): Option[Boolean] = {
    if (x < 0 || y < 0 || x > width || y > width) None
    else horizontal(y)(x)
  }

  def getBottom(x: Int, y: Int): Option[Boolean] = getTop(x, y + 1)

  def getLeft(x: Int, y: Int): Option[Boolean] = {
    if (x < 0 || y < 0 || x > width || y > width) None
    else vertical(y)(x)
  }

  def getRight(x: Int, y: Int): Option[Boolean] = getLeft(x + 1, y)

  def getNumber(x: Int, y: Int) = numbers(y)(x)

  // Get any number cell
  def getNumberCoords(): List[(Int, Int, Int)] = {
    numbers.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (num, x) =>
        if (num.isDefined) Some((x, y, num.get)) else None
      }
    }.filter(_.isDefined).map(c => c.get)
  }

  // Get specific number cell
  def getNumberCoords(n: Int): List[(Int, Int)] = {
    numbers.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (num, x) =>
        if (num.isDefined && num.get == n) Some((x, y)) else None
      }
    }.filter(_.isDefined).map(c => c.get)
  }

  def getSideCount(x: Int, y: Int) = {
    List(getTop(x, y), getRight(x, y), getBottom(x, y), getLeft(x, y)).count(_.contains(true))
  }

  def getSolvedSegments(x: Int = 0, y: Int = 0, dir: Int = 0, segments: List[List[(Int, Int)]] = List()): List[List[(Int, Int)]] = {
    if (y >= height) return segments

    val ndir = (dir + 1) % 4
    val (nx, ny) = if (ndir == 0) {
      val nx = if (x == width - 1) 0 else x + 1
      val ny = if (nx > x) y else y + 1
      (nx, ny)
    } else {
      (x, y)
    }

    val segment = ndir match {
      case 0 => if (getTop(x, y).contains(true)) Some(List((x, y), (x + 1, y))) else None
      case 1 => if (getRight(x, y).contains(true)) Some(List((x + 1, y), (x + 1, y + 1))) else None
      case 2 => if (getBottom(x, y).contains(true)) Some(List((x, y + 1), (x + 1, y + 1))) else None
      case 3 => if (getLeft(x, y).contains(true)) Some(List((x, y), (x, y + 1))) else None
      case _ => None
    }

    if (segment.isDefined) segment.get :: getSolvedSegments(nx, ny, ndir) else getSolvedSegments(nx, ny, ndir)
  }

  def setTop(x: Int, y: Int, value: Boolean) = {
    horizontal(y)(x) = Some(value)
    //val newRow = horizontal(y).take(x) ++ List(Some(value)) ++ horizontal(y).drop(x + 1)
    //horizontal = horizontal.take(y) ++ List(newRow) ++ horizontal.drop(y + 1)
  }

  def setBottom(x: Int, y: Int, value: Boolean) = {
    setTop(x, y + 1, value)
  }

  def setLeft(x: Int, y: Int, value: Boolean) = {
    vertical(y)(x) = Some(value)
    //val newCol = vertical(y).take(x) ++ List(Some(value)) ++ vertical(y).drop(x + 1)
    //vertical = vertical.take(y) ++ List(newCol) ++ vertical.drop(y + 1)
  }

  def setRight(x: Int, y: Int, value: Boolean) = {
    setLeft(x + 1, y, value)
  }

  def getLineUp(x: Int, y: Int) = {
    if (y > 0) vertical(y - 1)(x)
    else None
  }

  def getLineRight(x: Int, y: Int) = {
    horizontal(y)(x)
  }

  def getLineDown(x: Int, y: Int) = {
    vertical(y)(x)
  }

  def getLineLeft(x: Int, y: Int) = {
    if (x > 0) horizontal(y)(x - 1)
    else None
  }


  def setLineUp(x: Int, y: Int, value: Boolean): Unit = {
    if (y > 0) {
      vertical(y - 1)(x) = Some(value)

      if (getLineLeft(x, y).isEmpty) setLineLeft(x, y, false)
      if (getLineDown(x, y).isEmpty) setLineDown(x, y, false)
      if (getLineRight(x, y).isEmpty) setLineRight(x, y, false)
    }
  }

  def setLineRight(x: Int, y: Int, value: Boolean): Unit = {
    horizontal(y)(x) = Some(value)

    if (getLineUp(x, y).isEmpty) setLineUp(x, y, false)
    if (getLineLeft(x, y).isEmpty) setLineLeft(x, y, false)
    if (getLineDown(x, y).isEmpty) setLineDown(x, y, false)
  }

  def setLineDown(x: Int, y: Int, value: Boolean): Unit = {
    vertical(y)(x) = Some(value)

    if (getLineLeft(x, y).isEmpty) setLineLeft(x, y, false)
    if (getLineUp(x, y).isEmpty) setLineUp(x, y, false)
    if (getLineRight(x, y).isEmpty) setLineRight(x, y, false)
  }

  def setLineLeft(x: Int, y: Int, value: Boolean): Unit = {
    if (x > 0) {
      horizontal(y)(x - 1) = Some(value)

      if (getLineUp(x, y).isEmpty) setLineUp(x, y, false)
      if (getLineRight(x, y).isEmpty) setLineRight(x, y, false)
      if (getLineDown(x, y).isEmpty) setLineDown(x, y, false)
    }
  }

  // Stringify
  override def toString: String = {
    val board = (0 until height).map(y => {
      val horiz = (0 until width).map(x => {
        val top = getTop(x, y)
        if (top.isDefined) {
          if (top.get) "-" else " "
        } else {
          "?"
        }
      }).mkString("+")
      val verti = (0 to width).map(x => {
        val left = getLeft(x, y)
        if (left.isDefined) {
          if (left.get) "|" else " "
        } else {
          "?"
        }
      }).mkString(" ") // to or until?

      s"+$horiz+\n$verti"
    }).mkString("\n")

    val lastHoriz = (0 until width).map(x => {
      val bottom = getBottom(x, height - 1)
      if (bottom.isDefined) {
        if (bottom.get) "-" else " "
      } else {
        "?"
      }
    }).mkString("+")

    s"${width}x$height\n$board\n+$lastHoriz+"
  }
}
