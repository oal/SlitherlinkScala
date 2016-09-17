class Board(boardfile: List[String], startLine: Int, sideLength: Int) {
  // Slice board file to get appropriate lines
  val getBoard = boardfile.slice(startLine, startLine + sideLength)

  // Create row from text line and what line number it is
  val row = getBoard.zipWithIndex.map { case (line, lineNum) => new Row(line, lineNum) }

  // Simple helper for cleaner lookups
  def getSquare(x: Int, y: Int): Square = row(y).square(x)

  def setConnector(y: Int, x: Int, p: Symbol, s: Boolean, l: Boolean) = {
    getSquare(x, y).connector(p).set = s
    getSquare(x, y).connector(p).locked = l
    p match {
      case 'Up => if (y > 0) {
        getSquare(x, y - 1).down.set = s
        getSquare(x, y - 1).down.locked = l
      }
      case 'Down => if (y < row.size - 1) {
        getSquare(x, y + 1).up.set = s
        getSquare(x, y + 1).up.locked = l
      }
      case 'Left => if (x > 0) {
        getSquare(x - 1, y).right.set = s
        getSquare(x - 1, y).right.locked = l
      }
      case 'Right => if (x < row(y).square.size - 1) {
        getSquare(x + 1, y).left.set = s
        getSquare(x + 1, y).left.locked = l
      }
    }
  }
}

class Row(row: String, rownumber: Int) {
  val getRow = row
  val square = for (i <- row.split(" ").indices)
    yield new Square(i, rownumber, Utils.toInt(row.split(" ")(i)))
}

class Square(nx: Int, ny: Int, nvalue: Int) {
  val value = nvalue
  val x = nx
  val y = ny

  val connector = Map[Symbol, Connector](
    ('Up, new Connector(false)),
    ('Down, new Connector(false)),
    ('Left, new Connector(false)),
    ('Right, new Connector(false))
  )

  // So we can look up on attributes directly instead of map access.
  val up = connector('Up)
  val down = connector('Down)
  val left = connector('Left)
  val right = connector('Right)

  def isEmpty: Boolean = {
    val c = for (i <- connector if i._2.set) yield 1
    c.isEmpty
  }

  def isFull: Boolean = {
    if (value != -1) {
      val c = for (i <- connector if i._2.set) yield 1
      c.size == value
    }
    else
      false
  }
}

class Connector(nset: Boolean) {
  var set = nset
  var locked = false
}