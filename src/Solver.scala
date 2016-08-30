object Solver {
  type Cell = (Option[Int], Boolean, Boolean)

  class Board(val width: Int, val height: Int, val rows: List[List[Cell]]) {
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