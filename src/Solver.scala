

class Square(val number: Option[Int], val possibleSolutions: List[(Boolean, Boolean, Boolean, Boolean)]) {
  def isSolved = possibleSolutions.length == 1

  def setTopKnown(state: Boolean) = new Square(number, possibleSolutions.filter(p => p._1 == state))
  def setRightKnown(state: Boolean) = new Square(number, possibleSolutions.filter(p => p._2 == state))
  def setBottomKnown(state: Boolean) = new Square(number, possibleSolutions.filter(p => p._3 == state))
  def setLeftKnown(state: Boolean) = new Square(number, possibleSolutions.filter(p => p._4 == state))
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
               val squares: Array[Square],
               var horizontal: List[List[Boolean]],
               var vertical: List[List[Boolean]]
              ) {


    // Solve puzzle
    def solve(): Puzzle = {
      squares.foreach(s => print(s.possibleSolutions.length + ", "))
      println("\n\nOnes in corners")

      // Apply rules
      onesInCorners()
      squares.foreach(s => print(s.possibleSolutions.length + ", "))
      println("\n\nTwos in corners")

      twosInCorners()
      squares.foreach(s => print(s.possibleSolutions.length + ", "))
      println("\n\nThrees in corners")

      threesInCorners()
      squares.foreach(s => print(s.possibleSolutions.length + ", "))
      println("\n")

      this
    }

    // Rules
    def onesInCorners() = {
      if (getSquare(0, 0).number.contains(1)) {
        val topFiltered = getSquare(0, 0).setTopKnown(false)
        val topLeftFiltered = topFiltered.setLeftKnown(false)
        setSquare(0, 0, topLeftFiltered)
      }
      if (getSquare(width-1, 0).number.contains(1)) {
        val topFiltered = getSquare(width-1, 0).setTopKnown(false)
        val topRightFiltered = topFiltered.setRightKnown(false)
        setSquare(width-1, 0, topRightFiltered)
      }
      if (getSquare(0, height-1).number.contains(1)) {
        val bottomFiltered = getSquare(0, height-1).setBottomKnown(false)
        val bottomLeftFiltered = bottomFiltered.setLeftKnown(false)
        setSquare(0, height-1, bottomLeftFiltered)
      }
      if (getSquare(width-1, height-1).number.contains(1)) {
        val bottomFiltered = getSquare(width-1, height-1).setBottomKnown(false)
        val bottomRightFiltered = bottomFiltered.setRightKnown(false)
        setSquare(width-1, height-1, bottomRightFiltered)
      }
    }

    def twosInCorners() = {
      if (getSquare(0, 0).number.contains(2)) {
        setSquare(1, 0, getSquare(1, 0).setTopKnown(true))
        setSquare(0, 1, getSquare(0, 1).setLeftKnown(true))
      }
      if (getSquare(width-1, 0).number.contains(2)) {
        setSquare(width-2, 0, getSquare(width-2, 0).setTopKnown(true))
        setSquare(width-1, 1, getSquare(width-1, 1).setRightKnown(true))
      }
      if (getSquare(0, height-1).number.contains(2)) {
        setSquare(2, height-1, getSquare(2, height-1).setBottomKnown(true))
        setSquare(0, height-2, getSquare(0, height-2).setLeftKnown(true))
      }
      if (getSquare(width-1, height-1).number.contains(2)) {
        setSquare(width-2, height-1, getSquare(width-2, height-1).setBottomKnown(true))
        setSquare(width-1, height-2, getSquare(width-1, height-2).setRightKnown(true))
      }
    }

    def threesInCorners() = {
      if (getSquare(0, 0).number.contains(3)) {
        val topFiltered = getSquare(0, 0).setTopKnown(true)
        val topLeftFiltered = topFiltered.setLeftKnown(true)
        setSquare(0, 0, topLeftFiltered)
      }
      if (getSquare(width-1, 0).number.contains(3)) {
        val topFiltered = getSquare(width-1, 0).setTopKnown(true)
        val topRightFiltered = topFiltered.setRightKnown(true)
        setSquare(width-1, 0, topRightFiltered)
      }
      if (getSquare(0, height-1).number.contains(3)) {
        val bottomFiltered = getSquare(0, height-1).setBottomKnown(true)
        val bottomLeftFiltered = bottomFiltered.setLeftKnown(true)
        setSquare(0, height-1, bottomLeftFiltered)
      }
      if (getSquare(width-1, height-1).number.contains(3)) {
        val bottomFiltered = getSquare(width-1, height-1).setBottomKnown(true)
        val bottomRightFiltered = bottomFiltered.setRightKnown(true)
        setSquare(width-1, height-1, bottomRightFiltered)
      }
    }

    // Getters and setters
    def numLinesAt(x: Int, y: Int) = {
      List(horizontal(y)(x), horizontal(y + 1)(x), vertical(y)(x), vertical(y)(x + 1)).count _
    }

    def getSquare(x: Int, y: Int) = squares(y * width + x)

    def getTop(x: Int, y: Int) = horizontal(y)(x)

    def getBottom(x: Int, y: Int) = getTop(y + 1, x)

    def getLeft(x: Int, y: Int) = vertical(y)(x)

    def getRight(x: Int, y: Int) = getLeft(y, x + 1)

    def setSquare(x: Int, y: Int, square: Square) = squares(y * width + x) = square

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

    // ToString and parsing
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
    val numbers = lines.flatMap(line => line.split(" ").toList.map(s => Utils.toInt(s)))

    val squares = numbers.map(num => new Square(num, genSolutions(num))).toArray
    val line = List.fill(width)(false)
    val line2 = List.fill(width + 1)(false)
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