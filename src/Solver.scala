object Solver {
  type Cell = (Option[Int], Boolean, Boolean)

  class Board(val width: Int, val height: Int, val rows: List[List[Cell]]) {

    // Because apparently this is how scala does enums.
    object Side extends Enumeration {
      type Side = Value
      val Top, Right, Bottom, Left = Value
    }

    import Side._

    def solveStep(x: Int, y: Int, side: Side): Board = {
      side match {
        // Avoid special cases to handle, so we really only treat left and top
        case Right => solveStep(x + 1, y, Left)
        case Bottom => solveStep(x, y + 1, Top)
        case _ => {
          // Construct new cell based on side we wish to set line for.
          val newCell = (rows(y)(x)._1, true, false)

          // Construct new row from the rest of existing row, as well as the changed (new) cell.
          val newRow = rows(y).take(x) ++ List(newCell) ++ rows(y).drop(x + 1)

          // Assemble new board from old and new rows.
          val rowsBefore = rows.take(y)
          val rowsAfter = rows.drop(y + 1)
          val newRows = rowsBefore ++ List(newRow) ++ rowsAfter

          // Print progress (DEBUG)
          val newBoard = new Board(width, height, newRows)
          println(newBoard.toString)

          newBoard
        }
      }
    }

    def solve() = {
      solveStep(1, 1, Top)
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
        row.map(cell => if (cell._2) "-" else " ").mkString("+") + "+"
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

    new Board(width, height, cells ++ List(bottomCells))
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