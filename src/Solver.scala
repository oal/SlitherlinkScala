
object Solver {

  def possibleSolutionsN(n: Int) = {
    n match {
      case 0 => List(
        (false, false, false, false)
      )
      case 1 => List(
        (true, false, false, false),
        (false, true, false, false),
        (false, false, true, false),
        (false, false, false, true)
      )
      case 2 => List(
        (true, true, false, false),
        (false, false, true, true),
        (true, false, true, false),
        (true, false, false, true),
        (false, true, false, true),
        (false, true, true, false)
      )
      case 3 => List(
        (true, true, true, false),
        (true, false, true, true),
        (true, true, false, true),
        (false, true, true, true)
      )
      case 4 => List(
        (true, true, true, true)
      )
    }
  }

  case class Square(x: Int, y: Int, number: Option[Int], possibleSolutions: List[(Boolean, Boolean, Boolean, Boolean)] = List()) {
    val solutions = if (possibleSolutions.nonEmpty) possibleSolutions
    else if (number.isDefined) possibleSolutionsN(number.get)
    else (0 to 4).toList.flatMap(n => possibleSolutionsN(n))

    def isSolved = solutions.length == 1

    def setTop() = Square(x, y, number, solutions.filter(s => s._1))

    def setRight() = Square(x, y, number, solutions.filter(s => s._2))

    def setBottom() = Square(x, y, number, solutions.filter(s => s._3))

    def setLeft() = Square(x, y, number, solutions.filter(s => s._4))

    override def toString: String = {
      solutions.map(s => {
        s"+${if (s._1) "-" else " "}+\n${if (s._4) "|" else " "} ${if (s._2) "|" else " "}\n+${if (s._3) "-" else " "}+"
      }).mkString("\n\n")
    }
  }

  class Puzzle(val width: Int,
               val height: Int,
               val squares: List[Square]
              ) {


    def solve(): Puzzle = {
      this
    }

    def getSquare(x: Int, y: Int) = {
      squares.filter(s => s.x == x && s.y == y).head
    }

    override def toString: String = {
      s"${width}x$height\n${squares.map(s => s"${s.x}x${s.y} (${s.solutions.length} possible solutions)\n$s").mkString("\n_____\n")}"
    }
  }

  def parseBoard(width: Int, height: Int, lines: List[String]): Puzzle = {
    val numbers = lines.map(line => line.split(" ").toList.map(s => Utils.toInt(s)))

    val squares = (0 until height).flatMap(y => (0 until width).map(x => Square(x, y, numbers(y)(x))).toList).toList

    new Puzzle(width, height, squares)
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