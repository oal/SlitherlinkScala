class Board(boardfile: List[String], startLine: Int, sideLength: Int) {
  // Slice board file to get appropriate lines
  val getBoard = boardfile.slice(startLine, startLine + sideLength)

  // Create row from text line and what line number it is
  val row = getBoard.zipWithIndex.map { case (line, lineNum) => new Row(line, lineNum) }

  def setConnector(y: Int, x: Int, p: Symbol, s: Boolean, l: Boolean): Boolean = {
    // If already locked, and we try to set it, return false.
    if(row(y).square(x).connector(p).locked) return false

    row(y).square(x).connector(p).set = s
    row(y).square(x).connector(p).locked = l
    p match {
      case 'Up => if (y > 0) {
        row(y - 1).square(x).connector('Down).set = s
        row(y - 1).square(x).connector('Down).locked = l
      }
      case 'Down => if (y < row.size - 1) {
        row(y + 1).square(x).connector('Up).set = s
        row(y + 1).square(x).connector('Up).locked = l
      }
      case 'Left => if (x > 0) {
        row(y).square(x - 1).connector('Right).set = s
        row(y).square(x - 1).connector('Right).locked = l
      }
      case 'Right => if (x < row(y).square.size - 1) {
        row(y).square(x + 1).connector('Left).set = s
        row(y).square(x + 1).connector('Left).locked = l
      }
    }

    true
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