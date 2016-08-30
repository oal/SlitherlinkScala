import Solver.Side.Side

object Solver {
  type Cell = (Option[Int], Boolean, Boolean)

  // Because apparently this is how scala does enums.
  object Side extends Enumeration {
    type Side = Value
    val Top, Right, Bottom, Left = Value
  }

  class Board(val width: Int, val height: Int, val rows: List[List[Cell]], start: (Int, Int, Side)) {

    import Side._

    // Get number inside cell
    def getNumber(x: Int, y: Int): Option[Int] = {
      if (x < 0 || y < 0 || x > width || y > height) None
      else rows(y)(x)._1
    }

    // Getters to check if any side for this cell is set
    def getTop(x: Int, y: Int): Boolean = {
      if (x < 0 || y < 0 || x > width || y > height) false
      else rows(y)(x)._2
    }

    def getRight(x: Int, y: Int): Boolean = {
      if (x < 0 || y < 0 || x > width || y > height) false
      else rows(y)(x + 1)._3
    }

    def getBottom(x: Int, y: Int): Boolean = {
      if (x < 0 || y < 0 || x > width || y > height) false
      else rows(y + 1)(x)._2
    }

    def getLeft(x: Int, y: Int): Boolean = {
      if (x < 0 || y < 0 || x > width || y > height) false
      else rows(y)(x)._3
    }

    def getSide(x: Int, y: Int, side: Side): Boolean = {
      side match {
        case Top => getTop(x, y)
        case Right => getRight(x, y)
        case Bottom => getBottom(x, y)
        case Left => getLeft(x, y)
      }
    }

    def numSidesSet(x: Int, y: Int): Int = {
      List(getTop(x, y), getRight(x, y), getBottom(x, y), getLeft(x, y)).count(p => p)
    }

    // Recursive solver
    def solveStep(x: Int, y: Int, side: Side): Option[Board] = {
      side match {
        // Avoid special cases to handle, so we really only treat left and top
        case Right => solveStep(x + 1, y, Left)
        case Bottom => solveStep(x, y + 1, Top)
        case _ => {
          if(x == start._1 && y == start._2 && getSide(x, y, start._3)) return Option(this)

          /// Validation TODO: Extract validation to another function
          // Out of bounds?
          if (x < 0 || y < 0 || x > width || y > height) return None
          // Cell is zero?
          val maxSet = getNumber(x, y)
          if (maxSet contains 0) return None

          // Too many sides set?
          if (maxSet.orElse(Option(4)).get <= numSidesSet(x, y) + 1) return None

          /// Construct new board
          // Construct new cell based on side we wish to set line for.
          val cell = rows(y)(x) // Existing cell data
          val newCell = if (side == Top) (cell._1, true, cell._3) else (cell._1, cell._2, true)

          // Construct new row from the rest of existing row, as well as the changed (new) cell.
          val newRow = rows(y).take(x) ++ List(newCell) ++ rows(y).drop(x + 1)

          // Assemble new board from old and new rows.
          val rowsBefore = rows.take(y)
          val rowsAfter = rows.drop(y + 1)
          val newRows = rowsBefore ++ List(newRow) ++ rowsAfter

          // Print progress (DEBUG)
          val newBoard = new Board(width, height, newRows, start)
          println(newBoard)

          side match {
            case Top => newBoard.solveStep(x, y, Left)
            case Left => newBoard.solveStep(x, y, Bottom)
            case Bottom => newBoard.solveStep(x, y, Right)
            case Right => newBoard.solveStep(x + 1, y, Top).orElse(
              newBoard.solveStep(x, y - 1, Top).orElse(
                newBoard.solveStep(x - 1, y, Top).orElse(
                  newBoard.solveStep(x, y + 1, Top))))
          }
        }
      }
    }

    def solve(): Board = {
      // First, find a suitable start position (lowest possible number, but not 0).
      // Maybe there is a shorter way to achieve this?
      val minCell = rows.flatten.minBy(cell => {
        val num = cell._1.orElse(Option(0)).get
        if (num == 0) 5 // Set 0s to 5, as we never want to start at a 0
        else num // But we want to start at somewhere as low as possible
      })

      val idx = rows.flatten.indexOf(minCell)
      val x = idx % (width + 1)
      val y = idx / (height + 1)

      //val startBoard = new Board(width, height, rows, (x, y, Top))

      // Solve!
      println(s"Starting at ($x, $y)")
      new Board(width, height, rows, (x, y, Top)).solveStep(x, y, Top).orElse(
        new Board(width, height, rows, (x, y, Left)).solveStep(x, y, Left).orElse(
          new Board(width, height, rows, (x, y, Right)).solveStep(x, y, Right).orElse(
            new Board(width, height, rows, (x, y, Bottom)).solveStep(x, y, Bottom)))).get
    }

    def toStringInput: String = {
      // Map over every row, drop rightmost item as that is only used in computation of solution.
      // Output number if any, otherwise a star is output. Join cells by space, and rows by newline.
      val text = rows.map(row => row.dropRight(1).map(col => col._1 match {
        case Some(i) => i
        case None => "*"
      }).mkString(" ")).mkString("\n")

      // Output should be identical to the input.
      s"$width x $height\n$text\n"
    }

    override def toString: String = {
      // Prints even rows, with + between lines.
      def rowEven(row: List[Cell]) = {
        "+" + row.map(cell => if (cell._2) "-" else " ").mkString("+")
      }
      // Prints odd lines, with pipes and spaces.
      def rowOdd(row: List[Cell]) = {
        row.map(cell => if (cell._3) "|" else " ").mkString(" ")
      }

      // Print even version of row, then odd version to get both horizontal and
      // vertical lines between nodes / cells.
      val text = rows.map(row => rowEven(row) + "\n" + rowOdd(row)).mkString("\n")

      s"$width x $height\n$text\n"
    }
  }

  def parseBoard(width: Int, height: Int, lines: List[String]): Board = {
    // We need a row extra because its' top line will be the above's bottom line.
    // That's because cells only have top and left lines, so we need partial cells
    // on the right and bottom.
    val bottomCells: List[Cell] = List.fill(width + 1)((None, false, false))

    // Parse numbers to ints, or use None. Add one cell to the right (see above).
    val cells = lines.map(line => {
      line.split(" ").map(s => (Utils.toInt(s), false, false)).toList ++ List((None, false, false))
    })

    new Board(width, height, cells ++ List(bottomCells), (0, 0, Side.Top))
  }

  def parseBoards(lines: List[String]): List[Board] = {
    // Pattern match width / height
    val Array(width, height) = lines.head.split("x").map(s => s.toInt)
    val thisBoard = List(parseBoard(width, height, lines.tail.take(height)))

    // Recursively parse the rest of the boards
    val nextBoardData = lines.drop(height + 1) // +1 for the size line
    if (nextBoardData.nonEmpty) thisBoard ++ parseBoards(nextBoardData)
    else thisBoard
  }
}