import Direction._

class Puzzle(val width: Int,
             val height: Int,
             val numbers: List[List[Option[Int]]],
             val squares: SquareList,
             val start: (Int, Int),
             val link: List[Direction]) {

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
        countLines(x, y) == neededNum
      }
      else true
    }).find(el => !el)).find(el => el.isDefined && !el.get)

    matches.isEmpty
  }

  def canMove(cx: Int, cy: Int, dir: Direction, lastMove: Option[Direction]): Boolean = {
    dir match {
      case Up => cy > 0 && !lastMove.contains(Down) && squares.getSquare(cx, cy - 1).maybeLeft
      case Right => cx < width && !lastMove.contains(Left) && squares.getSquare(cx, cy).maybeTop
      case Down => cy < height && !lastMove.contains(Up) && squares.getSquare(cx, cy).maybeLeft
      case Left => cx > 0 && !lastMove.contains(Right) && squares.getSquare(cx - 1, cy).maybeTop
      case _ => false
    }
  }

  def solve(): Puzzle = {
    squares.applyRules()
    solveStep().get
  }

  // Recursive solver
  private def solveStep(): Option[Puzzle] = {
    val (cx, cy) = if (link.nonEmpty) {
      val curr = currPosition()
      val (x, y) = curr
      if (x < 0 || y < 0 || x > width || y > height) return None
      //if(countLines(x, y) > getNumber(x, y).getOrElse(4)) return None

      if (!lastMoveLegal()) return None

      if (curr == start) {
        if (validateNumbers()) return Option(this)
        else return None
      }

      (curr._1, curr._2)
    } else {
      start
    }

    // What moves are possible from the current position?
    val lastMove = link.lastOption
    val up = if (canMove(cx, cy, Up, lastMove)) List(Up) else List()
    val right = if (canMove(cx, cy, Right, lastMove)) List(Right) else List()
    val down = if (canMove(cx, cy, Down, lastMove)) List(Down) else List()
    val left = if (canMove(cx, cy, Left, lastMove)) List(Left) else List()

    // Build list, and process as stream until first match / solution is returned.
    val directions = List() ++ up ++ right ++ down ++ left

    directions.toStream.map(dir =>
      new Puzzle(width, height, numbers, squares, start, link ++ List(dir)).solveStep()
    ).collectFirst { case p if p.isDefined => p.get }
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
