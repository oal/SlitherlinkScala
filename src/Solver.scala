object Solver {
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

    val squares = numbers.flatten.zipWithIndex.map({
      case (num, i) => {
        val solutions = Utils.genSolutions(num)
        val (x, y) = (i % width, i / width)

        new Square(x, y, num, solutions)
      }
    }).toArray

    val squareList = new SquareList(width, height, squares)

    new Puzzle(width, height, numbers ++ List(bottomCells), squareList, (x, y), List())
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