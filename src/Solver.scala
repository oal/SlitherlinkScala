object Solver {
  type Cell = (Option[Int], Boolean, Boolean)

  class Board(val width: Int, val height: Int, val rows: List[List[Cell]]) {
    override def toString: String = {
      val text = rows.map(row => row.map(col => col._1 match {
        case Some(i) => i
        case None => "*"
      }).reduce((a, b) => s"$a $b")).reduce((a, b) => s"$a\n$b")
      s"$width x $height\n$text\n"
    }
  }

  def parseBoard(width: Int, height: Int, lines: List[String]): Board = {
    val cells = lines.map(line => {
      line.split(" ").map(s => (Utils.toInt(s), false, false)).toList
    })
    new Board(width, height, cells)
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