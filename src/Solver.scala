object Solver {
  def parseBoard(width: Int, height: Int, lines: List[String]): Puzzle = {
    // Parse numbers to ints, or use None. Add one cell to the right (see above).
    val numbers = lines.map(line => {
      line.split(" ").map(s => Utils.toInt(s)).toList
    })

    val board = new Board(width, height, numbers)

    new Puzzle(width, height, board)
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