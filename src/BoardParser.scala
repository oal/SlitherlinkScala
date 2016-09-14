import scala.io.Source

class BoardParser(path: String) {
  private val boardFile = Source.fromFile(path).getLines.toList

  // Tuples of start position of board, and side length
  private val startList = for (i <- boardFile if i.contains("x"))
    yield Tuple2(boardFile.indexOf(i) + 1, i.split("x").head.toInt)

  // Create Board instances from lines
  val numPuzzles = boardFile.head.toInt
  val getBoards = boardFile
  val board = for (i <- 0 until numPuzzles)
    yield new Board(boardFile, startList(i)._1, startList(i)._2)


  class Board(boardfile: List[String], startLine: Int, sideLength: Int) {
    // Slice board file to get appropriate lines
    val getBoard = boardfile.slice(startLine, startLine + sideLength)

    // Create row from text line and what line number it is
    val row = getBoard.zipWithIndex.map { case (line, lineNum) => new Row(line, lineNum) }
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

  def setConnector(b: Int, y: Int, x: Int, p: Symbol, s: Boolean, l: Boolean) = {
    board(b).row(y).square(x).connector(p).set = s
    board(b).row(y).square(x).connector(p).locked = l
    p match {
      case 'Up => if (y > 0) {
        board(b).row(y - 1).square(x).connector('Down).set = s
        board(b).row(y - 1).square(x).connector('Down).locked = l
      }
      case 'Down => if (y < board(b).row.size - 1) {
        board(b).row(y + 1).square(x).connector('Up).set = s
        board(b).row(y + 1).square(x).connector('Up).locked = l
      }
      case 'Left => if (x > 0) {
        board(b).row(y).square(x - 1).connector('Right).set = s
        board(b).row(y).square(x - 1).connector('Right).locked = l
      }
      case 'Right => if (x < board(b).row(y).square.size - 1) {
        board(b).row(y).square(x + 1).connector('Left).set = s
        board(b).row(y).square(x + 1).connector('Left).locked = l
      }
      case _ => board(b).row(y).square(x).connector.filterKeys(Set('Up, 'Down, 'Left, 'Right))
    }
  }

}