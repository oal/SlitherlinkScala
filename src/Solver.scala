import scala.annotation.tailrec
import scala.util.Random

class Square(val x: Int, val y: Int, val number: Option[Int], val possibleSolutions: List[(Boolean, Boolean, Boolean, Boolean)]) {
  def isSolved = possibleSolutions.length == 1

  def solution = possibleSolutions.head

  def setTopKnown(state: Boolean) = new Square(x, y, number, possibleSolutions.filter(p => p._1 == state))

  def setRightKnown(state: Boolean) = new Square(x, y, number, possibleSolutions.filter(p => p._2 == state))

  def setBottomKnown(state: Boolean) = new Square(x, y, number, possibleSolutions.filter(p => p._3 == state))

  def setLeftKnown(state: Boolean) = new Square(x, y, number, possibleSolutions.filter(p => p._4 == state))

  def maybeTop = possibleSolutions.exists(p => p._1)

  def maybeRight = possibleSolutions.exists(p => p._2)

  def maybeBottom = possibleSolutions.exists(p => p._3)

  def maybeLeft = possibleSolutions.exists(p => p._4)

  override def toString: String = s"$x x $y (${possibleSolutions.length} solutions): $possibleSolutions"
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

      // Update board based on what we know now.
      updateLinesWithSolutions()

      // Apply position dependent rules (none yet)
      for (y <- 0 until height) {
        for (x <- 0 until width) {
          threeAboveZero(x, y)
          threeNextToThree(x, y)
        }
      }

      bruteforce()
      this
    }

    // Rules
    def onesInCorners() = {
      if (getSquare(0, 0).number.contains(1)) {
        val topFiltered = getSquare(0, 0).setTopKnown(false)
        val topLeftFiltered = topFiltered.setLeftKnown(false)
        setSquare(0, 0, topLeftFiltered)
      }
      if (getSquare(width - 1, 0).number.contains(1)) {
        val topFiltered = getSquare(width - 1, 0).setTopKnown(false)
        val topRightFiltered = topFiltered.setRightKnown(false)
        setSquare(width - 1, 0, topRightFiltered)
      }
      if (getSquare(0, height - 1).number.contains(1)) {
        val bottomFiltered = getSquare(0, height - 1).setBottomKnown(false)
        val bottomLeftFiltered = bottomFiltered.setLeftKnown(false)
        setSquare(0, height - 1, bottomLeftFiltered)
      }
      if (getSquare(width - 1, height - 1).number.contains(1)) {
        val bottomFiltered = getSquare(width - 1, height - 1).setBottomKnown(false)
        val bottomRightFiltered = bottomFiltered.setRightKnown(false)
        setSquare(width - 1, height - 1, bottomRightFiltered)
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
      }
    }

    // Validation
    def evenLines(): Boolean = {
      if(!vertical.forall(line => line.count(_ == true) % 2 == 0)) false
      if(!(0 until width).forall(x => (0 to height).map(y => horizontal(y)(x)).count(_ == true) % 2 == 0)) false
      else true
    }

    def allHaveTwoNeighbors(): Boolean = {
      for(y <- 0 until height) {
        for(x <- 0 until width) {
          println(x, y, List(hasUp(x, y), hasRight(x, y), hasDown(x, y), hasLeft(x, y)))
          if(List(hasUp(x, y), hasRight(x, y), hasDown(x, y), hasLeft(x, y)).count(_==true) != 2) return false
        }
      }
      true
    }

    // Getters and setters
    def numLinesAt(x: Int, y: Int) = {
      List(horizontal(y)(x), horizontal(y + 1)(x), vertical(y)(x), vertical(y)(x + 1)).count _
    }

    def isOnBoard(x: Int, y: Int) = !(x < 0 || y < 0 || x >= width || y >= height)

    def getSquare(x: Int, y: Int) = squares(y * width + x)

    // Getters to check if a square has lines around it
    def getTop(x: Int, y: Int): Boolean = horizontal(y)(x)

    def getBottom(x: Int, y: Int): Boolean = getTop(x, y + 1)

    def getLeft(x: Int, y: Int): Boolean = vertical(y)(x)

    def getRight(x: Int, y: Int): Boolean = getLeft(x + 1, y)

    // Getters to check if a n "intersection" has lines going out from it
    def hasUp(x: Int, y: Int): Boolean = {
      if(y <= 0) false
      else vertical(y-1)(x)
    }
    def hasDown(x: Int, y: Int): Boolean = {
      if(y >= height) false
      else vertical(y)(x)
    }
    def hasLeft(x: Int, y: Int): Boolean = {
      if(x <= 0) false
      else horizontal(y)(x-1)
    }
    def hasRight(x: Int, y: Int): Boolean = {
      if(x >= width || y >= height) false
      else horizontal(y)(x)
    }

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

    def updateLinesWithSolutions() = {
      squares.foreach(s => if (s.isSolved) {
        setTop(s.x, s.y, s.solution._1)
        setRight(s.x, s.y, s.solution._2)
        setBottom(s.x, s.y, s.solution._3)
        setLeft(s.x, s.y, s.solution._4)
      })
    }

    @tailrec private def bruteforce():Unit = {
      val r = new Random()
      for (y <- 0 until height) {
        for (x <- 0 until width) {
          val s = getSquare(x, y)
          if (!s.isSolved) {
            // TODO: Consider what was chosen for neighbor tiles before choosing solution.
            val solution = s.possibleSolutions(r.nextInt(s.possibleSolutions.length))
            setTop(x, y, solution._1)
            setRight(x, y, solution._2)
            setBottom(x, y, solution._3)
            setLeft(x, y, solution._4)
          }
        }
      }

      if(evenLines() && allHaveTwoNeighbors()) return
      else bruteforce()
    }

    // ToString and parsing
    override def toString: String = {
      val board = (0 until height).map(y => {
        val horiz = (0 until width).map(x => if (getTop(x, y)) "-" else " ").mkString("+")
        val verti = (0 to width).map(x => if (getLeft(x, y)) "|" else " ").mkString(" ") // to or until?

        s"+$horiz+\n$verti"
      }).mkString("\n")

      val lastHoriz = (0 until width).map(x => if (getBottom(x, height - 1)) "-" else " ").mkString("+")

      s"${width}x$height\n$board\n+$lastHoriz+"
    }
  }

  def parseBoard(width: Int, height: Int, lines: List[String]): Puzzle = {
    val numbers = lines.flatMap(line => line.split(" ").toList.map(s => Utils.toInt(s)))

    val squares = numbers.zipWithIndex.map { case (num, i) => new Square(i % width, i / width, num, genSolutions(num)) }.toArray
    val line = List.fill(width)(false)
    val line2 = List.fill(height)(false)
    new Puzzle(width, height, squares, List.fill(height + 1)(line), List.fill(height)(line2))
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