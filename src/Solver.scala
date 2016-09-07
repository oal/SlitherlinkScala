import Solver.Direction.Direction

import scala.annotation.tailrec

object Solver {

  // Because apparently this is how scala does enums.
  object Direction extends Enumeration {
    type Direction = Value
    val Up, Down, Left, Right = Value
  }

  class Square(val x: Int, val y: Int, val number: Option[Int], val possibleSolutions: List[(Boolean, Boolean, Boolean, Boolean)]) {
    def isSolved = possibleSolutions.length == 1

    def solution = possibleSolutions.head

    def setTopKnown(state: Boolean) = new Square(x, y, number, possibleSolutions.filter(p => p._1 == state))

    def setRightKnown(state: Boolean) = new Square(x, y, number, possibleSolutions.filter(p => p._2 == state))

    def setBottomKnown(state: Boolean) = new Square(x, y, number, possibleSolutions.filter(p => p._3 == state))

    def setLeftKnown(state: Boolean) = new Square(x, y, number, possibleSolutions.filter(p => p._4 == state))

    def popSolution() = new Square(x, y, number, possibleSolutions.tail)

    def maybeTop = possibleSolutions.exists(p => p._1)

    def maybeRight = possibleSolutions.exists(p => p._2)

    def maybeBottom = possibleSolutions.exists(p => p._3)

    def maybeLeft = possibleSolutions.exists(p => p._4)

    override def toString: String = s"$x x $y (${possibleSolutions.length} solutions): $possibleSolutions"
  }

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
               val numbers: List[List[Option[Int]]],
               val squares: Array[Square],
               val start: (Int, Int),
               val link: List[Direction]) {

    import Direction._

    // Get number inside cell
    def getNumber(x: Int, y: Int): Option[Int] = {
      if (x < 0 || y < 0 || x > width || y > height) None
      else numbers(y)(x)
    }

    def currPosition(): (Int, Int) = {
      val (x, y) = link.map {
        case Up => (0, -1)
        case Down => (0, 1)
        case Left => (-1, 0)
        case Right => (1, 0)
        case _ => (0, 0)
      }.reduce((a, b) => (a._1 + b._1, a._2 + b._2))

      (x + start._1, y + start._2)
    }

    def linkCoords: List[(Int, Int)] = {
      val steps = link.map {
        case Up => (0, -1)
        case Down => (0, 1)
        case Left => (-1, 0)
        case Right => (1, 0)
        case _ => (0, 0)
      }

      val withStart: List[(Int, Int)] = start :: steps
      withStart.drop(1).scanLeft(withStart.head) {
        case (r, c) => (r._1 + c._1, r._2 + c._2)
      }
    }

    def lineSegments(): List[List[(Int, Int)]] = {
      linkCoords.sliding(2).toList
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

    def isOnBoard(x: Int, y: Int) = !(x < 0 || y < 0 || x >= width || y >= height)

    def getSquare(x: Int, y: Int) = {
      //if (x < 0 || y < 0 || x >= width || y >= height) println("Square out of bounds!")
      squares(y * width + x)
    }

    def setSquare(x: Int, y: Int, square: Square) = squares(y * width + x) = square

    def countLines(x: Int, y: Int): Int = {
      val els = List(
        List((x, y), (x + 1, y)),
        List((x + 1, y), (x, y)),

        List((x, y), (x, y + 1)),
        List((x, y + 1), (x, y)),

        List((x + 1, y), (x + 1, y + 1)),
        List((x + 1, y + 1), (x + 1, y)),

        List((x, y + 1), (x + 1, y + 1)),
        List((x + 1, y + 1), (x, y + 1))
      ).toSet

      lineSegments().toSet.intersect(els).size
    }

    def lastMoveLegal(): Boolean = {
      val segments = lineSegments()

      val last = segments.last
      val lastReverse = List(last(1), last.head)

      val rest = segments.drop(1).dropRight(1).toSet
      val checkSegments = Set(
        last,
        lastReverse,
        List(last(1), (last(1)._1 + 1, last(1)._2)),
        List(last(1), (last(1)._1, last(1)._2 + 1)),
        List(last(1), (last(1)._1 - 1, last(1)._2)),
        List(last(1), (last(1)._1, last(1)._2 - 1))
      )

      rest.intersect(checkSegments).isEmpty
    }

    def validateNumbers(): Boolean = {
      val matches = (0 until height).map(y => (0 until width).map(x => {
        val num = getNumber(x, y)
        if (num.isDefined) {
          val neededNum = num.get
          //println(x, y, neededNum, countLines(x, y))
          countLines(x, y) == neededNum
        }
        else true
      }).find(el => !el)).find(el => el.isDefined && !el.get)

      matches.isEmpty
    }

    def canMove(cx: Int, cy: Int, dir: Direction, lastMove: Option[Direction]): Boolean = {
      dir match {
        case Up => cy > 0 && !lastMove.contains(Down) && getSquare(cx, cy - 1).maybeLeft
        case Right => cx < width && !lastMove.contains(Left) && getSquare(cx, cy).maybeTop
        case Down => cy < height && !lastMove.contains(Up) && getSquare(cx, cy).maybeLeft
        case Left => cx > 0 && !lastMove.contains(Right) && getSquare(cx - 1, cy).maybeTop
        case _ => false
      }
    }

    // Recursive solver
    private def solveStep(): Option[Puzzle] = {
      val (cx, cy) = if (link.nonEmpty) {
        val curr = currPosition()
        val (x, y) = curr
        if (x < 0 || y < 0 || x > width || y > height) return None

        if (!lastMoveLegal()) return None

        if (curr == start) {
          if (validateNumbers()) return Option(this)
          else return None
        }

        (curr._1, curr._2)
      } else {
        start
      }

      val lastMove = link.lastOption
      val up = if (canMove(cx, cy, Up, lastMove)) List(Up) else List() //if(!lastMove.contains(Down) && cx < width && getSquare(cx, cy).maybeTop) List(Up) else List()
      val right = if (canMove(cx, cy, Right, lastMove)) List(Right) else List() //if(!lastMove.contains(Left) && cx < width && getSquare(cx, cy).maybeTop) List(Right) else List()
      val down = if (canMove(cx, cy, Down, lastMove)) List(Down) else List() //if(!lastMove.contains(Up) && cx < width && getSquare(cx, cy).maybeLeft) List(Down) else List()
      val left = if (canMove(cx, cy, Left, lastMove)) List(Left) else List() //if(!lastMove.contains(Right) && cx < width && getSquare(cx, cy).maybeTop) List(Left) else List()

      // Not pretty, but ... recursion.
      val directions = List() ++ up ++ right ++ down ++ left
      directions.length match {
        case 1 => new Puzzle(width, height, numbers, squares, start, link ++ List(directions.head)).solveStep()
        case 2 => new Puzzle(width, height, numbers, squares, start, link ++ List(directions.head)).solveStep().orElse(
          new Puzzle(width, height, numbers, squares, start, link ++ List(directions(1))).solveStep()
        )
        case 3 => new Puzzle(width, height, numbers, squares, start, link ++ List(directions.head)).solveStep().orElse(
          new Puzzle(width, height, numbers, squares, start, link ++ List(directions(1))).solveStep().orElse(
            new Puzzle(width, height, numbers, squares, start, link ++ List(directions(2))).solveStep()
          )
        )
        case 4 => new Puzzle(width, height, numbers, squares, start, link ++ List(directions.head)).solveStep().orElse(
          new Puzzle(width, height, numbers, squares, start, link ++ List(directions(1))).solveStep().orElse(
            new Puzzle(width, height, numbers, squares, start, link ++ List(directions(2))).solveStep().orElse(
              new Puzzle(width, height, numbers, squares, start, link ++ List(directions(3))).solveStep()
            )
          )
        )
        case _ => None
      }
    }

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

      // Apply position dependent rules (none yet)
      for (y <- 0 until height) {
        for (x <- 0 until width) {
          threeAboveZero(x, y)
          threeNextToThree(x, y)
        }
      }

      solveStep().get

      /*new Puzzle(width, height, numbers, squares, start, List(Up)).solveStep().orElse(
        new Puzzle(width, height, numbers, squares, start, List(Right)).solveStep().orElse(
          new Puzzle(width, height, numbers, squares, start, List(Down)).solveStep().orElse(
            new Puzzle(width, height, numbers, squares, start, List(Left)).solveStep()
          )
        )
      ).get*/
    }

    def toStringInput: String = {
      // Map over every row, drop rightmost item as that is only used in computation of solution.
      // Output number if any, otherwise a star is output. Join cells by space, and rows by newline.
      val text = numbers.map(row => row.dropRight(1).map {
        case Some(i) => i
        case None => "*"
      }.mkString(" ")).mkString("\n")

      // Output should be identical to the input.
      s"$width x $height\n$text\n"
    }

    override def toString: String = {
      // Create pairs of two points: Each end of a line segment.
      val slidingCoords = lineSegments()

      // Filter out and organize horizontal and vertical lines.
      val horizontalLines: List[(Int, Int)] = slidingCoords.
        filter(pair => pair.head._2 == pair(1)._2).
        map(pair => (Math.min(pair.head._1, pair(1)._1), pair(1)._2))

      val verticalLines: List[(Int, Int)] = slidingCoords.
        filter(pair => pair.head._1 == pair(1)._1).
        map(pair => (pair(1)._1, Math.min(pair.head._2, pair(1)._2)))

      // Construct nested lists of strings based on where we have lines or not.
      val out = List.fill(height + 1)("").zipWithIndex.map {
        case (s, iy: Int) =>
          List(
            List.fill(width + 1)("").zipWithIndex.map {
              case (s2, ix: Int) => {
                if (horizontalLines.count { case (x, y) => x == ix && y == iy } > 0) "+-"
                else "+ "
              }
              case _ => "+ "
            },
            List.fill(width + 1)("").zipWithIndex.map {
              case (s2, ix: Int) => {
                if (verticalLines.count { case (x, y) => x == ix && y == iy } > 0) "| "
                else "  "
              }
              case _ => "  "
            }
          )
      }

      // Combine all the small strings to one, with line breaks etc.
      val asString = out.map(doubleLine => doubleLine.map(
        line => line.mkString("")).mkString("\n")).mkString("\n")

      // Provide correct output. Trim to avoid line with only spaces at the end.
      s"$width x $height\n${asString.trim}"
    }
  }

  def parseBoard(width: Int, height: Int, lines: List[String]): Puzzle = {
    // We need a row extra because its' top line will be the above's bottom line.
    // That's because cells only have top and left lines, so we need partial cells
    // on the right and bottom.
    val bottomCells: List[Option[Int]] = List.fill(width + 1)(None)

    // Parse numbers to ints, or use None. Add one cell to the right (see above).
    val numbers = lines.map(line => {
      line.split(" ").map(s => Utils.toInt(s)).toList ++ List(None)
    })

    // First, find a suitable start position (lowest possible number, but not 0).
    // Maybe there is a shorter way to achieve this?
    val minCell = numbers.flatten.minBy(cell => {
      val num = cell.orElse(Option(0)).get
      if (num == 0) 5 // Set 0s to 5, as we never want to start at a 0
      else num // But we want to start at somewhere as low as possible
    })

    val idx = numbers.flatten.indexOf(minCell)
    val x = idx % (width + 1)
    val y = idx / (height + 1)

    val squares = numbers.flatten.zipWithIndex.map { case (num, i) => new Square(i % width, i / width, num, genSolutions(num)) }.toArray

    new Puzzle(width, height, numbers ++ List(bottomCells), squares, (x, y), List())
  }

  def parseBoards(lines: List[String]): List[Puzzle] = {
    // Pattern match width / height
    val Array(width, height) = lines.head.split("x").map(s => s.toInt)
    val thisBoard = List(parseBoard(width, height, lines.tail.take(height)))

    // Recursively parse the rest of the boards
    val nextBoardData = lines.drop(height + 1) // +1 for the size line
    if (nextBoardData.nonEmpty) thisBoard ++ parseBoards(nextBoardData)
    else thisBoard
  }
}