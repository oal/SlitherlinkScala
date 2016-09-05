

class Square(val number: Option[Int], possibleSolutions: List[(Boolean, Boolean, Boolean, Boolean)]) {
  def isSolved = possibleSolutions.length == 1
}


object Solver {
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

  class Puzzle(val width: Int,
               val height: Int,
               val squares: List[List[Square]],
               var horizontal: List[List[Boolean]],
               var vertical: List[List[Boolean]]
              ) {


    def solve(): Puzzle = {
      setTop(4, 0, true)
      setLeft(1, 0, true)
      setRight(4, 1, true)
      println(numLinesAt(0, 0))
      this
    }

    def numLinesAt(x: Int, y: Int) = {
      List(horizontal(y)(x), horizontal(y + 1)(x), vertical(y)(x), vertical(y)(x + 1)).count _
    }

    def getSquare(x: Int, y: Int) = squares(y)(x)

    def getTop(x: Int, y: Int) = horizontal(y)(x)

    def getBottom(x: Int, y: Int) = getTop(y + 1, x)

    def getLeft(x: Int, y: Int) = vertical(y)(x)

    def getRight(x: Int, y: Int) = getLeft(y, x + 1)

    def setTop(x: Int, y: Int, value: Boolean) = {
      val newRow = horizontal(y).take(x) ++ List(value) ++ horizontal(y).drop(x + 1)
      horizontal = horizontal.take(y) ++ List(newRow) ++ horizontal.drop(y + 1)
    }

    def setBottom(x: Int, y: Int, value: Boolean) = {
      setTop(x, y + 1, value)
    }

    def setLeft(x: Int, y: Int, value: Boolean) = {
      val newCol = vertical(y).take(x) ++ List(value) ++ vertical(y).drop(x + 1)
      vertical = vertical.take(y) ++ List(newCol) ++ vertical.drop(y + 1)
    }

    def setRight(x: Int, y: Int, value: Boolean) = {
      setLeft(x + 1, y, value)
    }

    override def toString: String = {
      println(vertical(1))
      val board = (0 until height).map(y => {
        val horiz = (0 until width).map(x => if (getTop(x, y)) "-" else " ").mkString("+")
        val verti = (0 to width).map(x => if (getLeft(x, y)) "|" else " ").mkString(" ")

        s"+$horiz+\n$verti"
      }).mkString("\n")

      s"${width}x$height\n$board"
    }
  }

  def parseBoard(width: Int, height: Int, lines: List[String]): Puzzle = {
    val numbers = lines.map(line => line.split(" ").toList.map(s => Utils.toInt(s)))

    val squares = numbers.map(line => line.map(num => new Square(num, genSolutions(num))))
    val line = List.fill(width)(false)
    val line2 = List.fill(width+1)(false)
    new Puzzle(width, height, squares, List.fill(height)(line), List.fill(height)(line2))
  }

  def parseBoards(lines: List[String]): List[Puzzle] = {
    // Pattern match width / height
    val Array(width, height) = lines.head.split("x").map(s => s.toInt)
    parseBoard(width, height, lines.tail.take(height))
    val thisBoard = List(parseBoard(width, height, lines.tail.take(height)))

    // Recursively parse the rest of the boards
    val nextBoardData = lines.drop(height + 1) // +1 for the size line
    if (nextBoardData.nonEmpty) thisBoard ++ parseBoards(nextBoardData)
    else thisBoard
  }
}