import scala.io.Source

class BoardParser(path: String) {
  private val boardFile = Source.fromFile(path).getLines.toList

  // Tuples of start position of board, and side length
  private val startList = for (i <- boardFile if i.contains("x"))
    yield Tuple2(boardFile.indexOf(i) + 1, i.split("x").head.toInt)

  // Create Board instances from lines
  val numPuzzles = boardFile.head.toInt
  val getBoards = boardFile
  val board = for (i <- 0 until numPuzzles)
    yield new Board(boardFile, startList(i)._1, startList(i)._2)
}