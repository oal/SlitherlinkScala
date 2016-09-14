// This file is mostly low level setting and getting of lines and sides.
// A bit terse. Solving logic is in Solver.scala and Puzzle.scala.

class Board(val width: Int,
            val height: Int,
            val numbers: List[List[Option[Int]]],
            var vertical: List[List[Option[Boolean]]],
            var horizontal: List[List[Option[Boolean]]]) {

  // Make a full copy of board in its current state
  def copy() = {
    new Board(
      width, height, numbers,
      vertical,//.map(l => l.clone()),
      horizontal//.map(l => l.clone())
    )
  }

  // Has all sides of this square either been set to true or false?
  def isFinishedProcessing(x: Int, y: Int) = {
    getTop(x, y).isDefined && getRight(x, y).isDefined && getBottom(x, y).isDefined && getLeft(x, y).isDefined
  }

  // Get any number cell
  def getNumberCoords(): List[(Int, Int, Int)] = {
    numbers.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (num, x) =>
        if (num.isDefined) Some((x, y, num.get)) else None
      }
    }.filter(_.isDefined).map(c => c.get)
  }

  // Get specific number cells
  def getNumberCoords(n: Int): List[(Int, Int)] = {
    numbers.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (num, x) =>
        if (num.isDefined && num.get == n) Some((x, y)) else None
      }
    }.filter(_.isDefined).map(c => c.get)
  }

  // Count sides set around a square
  def getSideCount(x: Int, y: Int) = {
    List(getTop(x, y), getRight(x, y), getBottom(x, y), getLeft(x, y)).count(_.contains(true))
  }

  // List of all segments solved so far (run after rules are applied)
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

  // Get sides (for square)
  @inline def getTop(x: Int, y: Int): Option[Boolean] = {
    if (x < 0 || y < 0 || x > width || y > width) None
    else horizontal(y)(x)
  }

  @inline def getBottom(x: Int, y: Int): Option[Boolean] = getTop(x, y + 1)

  @inline def getLeft(x: Int, y: Int): Option[Boolean] = {
    if (x < 0 || y < 0 || x > width || y > width) None
    else vertical(y)(x)
  }

  @inline def getRight(x: Int, y: Int): Option[Boolean] = getLeft(x + 1, y)

  @inline def getNumber(x: Int, y: Int) = numbers(y)(x)

  // Set sides (for squares)
  @inline def setTop(x: Int, y: Int, value: Boolean) = {
    if (x < 0 || y < 0 || x > width || y > width) this
    else setHorizontal(x, y, value)
  }

  @inline def setBottom(x: Int, y: Int, value: Boolean) = {
    if (x < 0 || y < 0 || x > width || y > height-1) this
    else setHorizontal(x, y+1, value)
  }

  @inline def setLeft(x: Int, y: Int, value: Boolean) = {
    if (x < 0 || y < 0 || x > width || y > width) this
    else setVertical(x, y, value)
  }

  @inline def setRight(x: Int, y: Int, value: Boolean) = {
    if (x < 0 || y < 0 || x > width-1 || y > width) this
    else setVertical(x+1, y, value)
  }

  def setVertical(x: Int, y: Int, value: Boolean) = {
    val newCol = vertical(y).take(x) ++ List(Some(value)) ++ vertical(y).drop(x + 1)
    new Board(width, height, numbers, vertical.take(y) ++ List(newCol) ++ vertical.drop(y + 1), horizontal)
  }

  def setHorizontal(x: Int, y: Int, value: Boolean) = {
    val newRow = horizontal(y).take(x) ++ List(Some(value)) ++ horizontal(y).drop(x + 1)
    new Board(width, height, numbers, vertical, horizontal.take(y) ++ List(newRow) ++ horizontal.drop(y + 1))
  }

  // Get lines (from dots)
  @inline def getLineUp(x: Int, y: Int) = {
    if (y > 0) vertical(y - 1)(x)
    else None
  }

  @inline def getLineRight(x: Int, y: Int) = {
    horizontal(y)(x)
  }

  @inline def getLineDown(x: Int, y: Int) = {
    vertical(y)(x)
  }

  @inline def getLineLeft(x: Int, y: Int) = {
    if (x > 0) horizontal(y)(x - 1)
    else None
  }

  // Set lines (from dots)
  def setLineUp(x: Int, y: Int, value: Boolean): Board = {
    if (y > 0) {
      //vertical(y - 1)(x) = Some(value)
      val newBoard = setVertical(x, y-1, value)

      val newBoard2 = if (newBoard.getLineLeft(x, y).isEmpty) newBoard.setLineLeft(x, y, false) else newBoard
      val newBoard3 = if (newBoard2.getLineDown(x, y).isEmpty) newBoard2.setLineDown(x, y, false) else newBoard2
      val newBoard4 = if (newBoard3.getLineRight(x, y).isEmpty) newBoard3.setLineRight(x, y, false) else newBoard3
      newBoard4
    }
    else this
  }

  def setLineRight(x: Int, y: Int, value: Boolean): Board = {
    //horizontal(y)(x) = Some(value)
    val newBoard = setHorizontal(x, y, value)

    val newBoard2 = if (newBoard.getLineUp(x, y).isEmpty) newBoard.setLineUp(x, y, false) else newBoard
    val newBoard3 = if (newBoard2.getLineLeft(x, y).isEmpty) newBoard2.setLineLeft(x, y, false) else newBoard2
    val newBoard4 = if (newBoard3.getLineDown(x, y).isEmpty) newBoard3.setLineDown(x, y, false) else newBoard3
    newBoard4
  }

  def setLineDown(x: Int, y: Int, value: Boolean): Board = {
    //vertical(y)(x) = Some(value)
    val newBoard = setVertical(x, y, value)

    val newBoard2 = if (newBoard.getLineLeft(x, y).isEmpty) newBoard.setLineLeft(x, y, false) else newBoard
    val newBoard3 = if (newBoard2.getLineUp(x, y).isEmpty) newBoard2.setLineUp(x, y, false) else newBoard2
    val newBoard4 = if (newBoard3.getLineRight(x, y).isEmpty) newBoard3.setLineRight(x, y, false) else newBoard3
    newBoard4
  }

  def setLineLeft(x: Int, y: Int, value: Boolean): Board = {
    if (x > 0) {
      //horizontal(y)(x - 1) = Some(value)
      val newBoard = setHorizontal(x-1, y, value)

      val newBoard2 = if (newBoard.getLineUp(x, y).isEmpty) newBoard.setLineUp(x, y, false) else newBoard
      val newBoard3 = if (newBoard2.getLineRight(x, y).isEmpty) newBoard2.setLineRight(x, y, false) else newBoard2
      val newBoard4 = if (newBoard3.getLineDown(x, y).isEmpty) newBoard3.setLineDown(x, y, false) else newBoard3
      newBoard4
    }
    else this
  }

  // Stringify
  override def toString: String = {
    val board = (0 until height).map(y => {
      val horiz = (0 until width).map(x => {
        val top = getTop(x, y)
        if (top.isDefined) {
          if (top.get) "-" else " "
        } else {
          " "
        }
      }).mkString("+")
      val verti = (0 to width).map(x => {
        val left = getLeft(x, y)
        if (left.isDefined) {
          if (left.get) "|" else " "
        } else {
          " "
        }
      }).mkString(" ")

      s"+$horiz+\n$verti"
    }).mkString("\n")

    val lastHoriz = (0 until width).map(x => {
      val bottom = getBottom(x, height - 1)
      if (bottom.isDefined) {
        if (bottom.get) "-" else " "
      } else {
        " "
      }
    }).mkString("+")

    s"${width}x$height\n$board\n+$lastHoriz+\n"
  }
}
